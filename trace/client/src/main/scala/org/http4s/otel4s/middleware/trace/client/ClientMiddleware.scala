/*
 * Copyright 2023 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s
package otel4s.middleware
package trace
package client

import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Outcome
import cats.effect.Resource
import cats.syntax.applicative._
import cats.syntax.flatMap._
import org.http4s.client.Client
import org.typelevel.ci.CIString
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

/** Middleware for wrapping an http4s `Client` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client]]
  */
object ClientMiddleware {

  /** @return a client middleware builder with default configuration */
  def default[F[_]: Tracer: Concurrent](urlRedactor: UriRedactor): Builder[F] =
    new Builder[F](
      urlRedactor,
      Defaults.spanAndAttributeProvider,
      Defaults.urlTemplateClassifier,
      Defaults.allowedRequestHeaders,
      Defaults.allowedResponseHeaders,
      Defaults.shouldTrace,
    )

  /** The default configuration values for a client middleware builder. */
  object Defaults {
    def spanAndAttributeProvider: SpanAndAttributeProvider =
      SpanAndAttributeProvider.default
    def urlTemplateClassifier: UriTemplateClassifier =
      UriTemplateClassifier.indeterminate
    def allowedRequestHeaders: Set[CIString] =
      TypedAttributes.defaultAllowedHttpHeaders
    def allowedResponseHeaders: Set[CIString] =
      TypedAttributes.defaultAllowedHttpHeaders
    val shouldTrace: RequestPrelude => ShouldTrace = _ => ShouldTrace.Trace
  }

  /** A builder for client middlewares. */
  final class Builder[F[_]: Tracer: Concurrent] private[ClientMiddleware] (
      urlRedactor: UriRedactor,
      spanAndAttributeProvider: SpanAndAttributeProvider,
      urlTemplateClassifier: UriTemplateClassifier,
      allowedRequestHeaders: Set[CIString],
      allowedResponseHeaders: Set[CIString],
      shouldTrace: RequestPrelude => ShouldTrace,
  ) {
    private def copy(
        spanAndAttributeProvider: SpanAndAttributeProvider = this.spanAndAttributeProvider,
        urlTemplateClassifier: UriTemplateClassifier = this.urlTemplateClassifier,
        allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
        allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
        shouldTrace: RequestPrelude => ShouldTrace = this.shouldTrace,
    ): Builder[F] =
      new Builder[F](
        this.urlRedactor,
        spanAndAttributeProvider,
        urlTemplateClassifier,
        allowedRequestHeaders,
        allowedResponseHeaders,
        shouldTrace,
      )

    /** Sets how a span's name and `Attributes` are derived from a request and
      * response.
      */
    def withSpanAndAttributeProvider(
        spanAndAttributeProvider: SpanAndAttributeProvider
    ): Builder[F] =
      copy(spanAndAttributeProvider = spanAndAttributeProvider)

    // TODO: docs
    def withUrlTemplateClassifier(urlTemplateClassifier: UriTemplateClassifier): Builder[F] =
      copy(urlTemplateClassifier = urlTemplateClassifier)

    /** Sets which request headers are allowed to made into `Attribute`s. */
    def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): Builder[F] =
      copy(allowedRequestHeaders = allowedHeaders)

    /** Sets which response headers are allowed to made into `Attribute`s. */
    def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): Builder[F] =
      copy(allowedResponseHeaders = allowedHeaders)

    /** Sets how to determine when to trace a request and its response. */
    def withShouldTrace(shouldTrace: RequestPrelude => ShouldTrace): Builder[F] =
      copy(shouldTrace = shouldTrace)

    /** @return the configured middleware */
    def build: Client[F] => Client[F] = (client: Client[F]) =>
      Client[F] { (req: Request[F]) => // Resource[F, Response[F]]
        if (
          !shouldTrace(req.requestPrelude).shouldTrace ||
          !Tracer[F].meta.isEnabled
        ) {
          client.run(req)
        } else {
          val shared =
            spanAndAttributeProvider.processSharedData(req, urlTemplateClassifier, urlRedactor)
          val spanName =
            spanAndAttributeProvider.spanName(req, urlTemplateClassifier, urlRedactor, shared)
          val reqAttributes =
            spanAndAttributeProvider.requestAttributes(
              req,
              urlTemplateClassifier,
              urlRedactor,
              shared,
              allowedRequestHeaders,
            )

          MonadCancelThrow[Resource[F, *]].uncancelable { poll =>
            for {
              res <- Tracer[F]
                .spanBuilder(spanName)
                .withSpanKind(SpanKind.Client)
                .addAttributes(reqAttributes)
                .build
                .resource
              span = res.span
              trace = res.trace
              traceHeaders <- Resource.eval(Tracer[F].propagate(Headers.empty)).mapK(trace)
              newReq = req.withHeaders(traceHeaders ++ req.headers)

              resp <- poll(client.run(newReq).mapK(trace)).guaranteeCase {
                case Outcome.Succeeded(fa) =>
                  fa.evalMap { resp =>
                    val respAttributes =
                      spanAndAttributeProvider.responseAttributes(
                        resp,
                        allowedResponseHeaders,
                      )
                    span.addAttributes(respAttributes) >> span
                      .setStatus(StatusCode.Error)
                      .unlessA(resp.status.isSuccess)
                  }

                case Outcome.Errored(e) =>
                  Resource.eval(span.addAttributes(TypedAttributes.errorType(e)))

                case Outcome.Canceled() =>
                  Resource.unit
              }
            } yield resp
          }
        }
      }
  }

}
