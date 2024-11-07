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
package server

import cats.data.Kleisli
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Outcome
import cats.effect.syntax.monadCancel._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import org.typelevel.ci.CIString
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

/** Middleware for wrapping an http4s `Server` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
  */
object ServerMiddleware {

  /** @return a server middleware builder with default configuration */
  def default[F[_]: Tracer: MonadCancelThrow](
      redactor: PathAndQueryRedactor
  ): Builder[F] =
    new Builder[F](
      redactor,
      Defaults.spanAndAttributeProvider,
      Defaults.routeClassifier,
      Defaults.allowedRequestHeaders,
      Defaults.allowedResponseHeaders,
      Defaults.shouldTrace,
    )

  /** The default configuration values for a server middleware builder. */
  object Defaults {
    def spanAndAttributeProvider: SpanAndAttributeProvider =
      SpanAndAttributeProvider.default
    val routeClassifier: RouteClassifier = RouteClassifier.indeterminate
    def allowedRequestHeaders: Set[CIString] =
      TypedAttributes.defaultAllowedHttpHeaders
    def allowedResponseHeaders: Set[CIString] =
      TypedAttributes.defaultAllowedHttpHeaders
    val shouldTrace: RequestPrelude => ShouldTrace = _ => ShouldTrace.Trace
  }

  /** A builder for server middlewares. */
  final class Builder[F[_]: Tracer: MonadCancelThrow] private[ServerMiddleware] (
      redactor: PathAndQueryRedactor, // cannot safely have default value
      spanAndAttributeProvider: SpanAndAttributeProvider,
      routeClassifier: RouteClassifier,
      allowedRequestHeaders: Set[CIString],
      allowedResponseHeaders: Set[CIString],
      shouldTrace: RequestPrelude => ShouldTrace,
  ) {
    private def copy(
        spanAndAttributeProvider: SpanAndAttributeProvider = this.spanAndAttributeProvider,
        routeClassifier: RouteClassifier = this.routeClassifier,
        allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
        allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
        shouldTrace: RequestPrelude => ShouldTrace = this.shouldTrace,
    ): Builder[F] =
      new Builder[F](
        this.redactor,
        spanAndAttributeProvider,
        routeClassifier,
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

    /** Sets how to determine the route within the application from a request. */
    def withRouteClassifier(routeClassifier: RouteClassifier): Builder[F] =
      copy(routeClassifier = routeClassifier)

    /** Sets which request headers are allowed to made into `Attribute`s. */
    def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): Builder[F] =
      copy(allowedRequestHeaders = allowedHeaders)

    /** Sets which response headers are allowed to made into `Attribute`s. */
    def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): Builder[F] =
      copy(allowedResponseHeaders = allowedHeaders)

    /** Sets how to determine when to trace a request and its response. */
    def withShouldTrace(shouldTrace: RequestPrelude => ShouldTrace): Builder[F] =
      copy(shouldTrace = shouldTrace)

    /** Returns a middleware in a way that abstracts over
      * [[org.http4s.HttpApp `HttpApp`]] and
      * [[org.http4s.HttpRoutes `HttpRoutes`]]. In most cases, it is preferable
      * to use the methods that directly build the specific desired type.
      *
      * @see [[buildHttpApp]]
      * @see [[buildHttpRoutes]]
      */
    def buildGenericTracedHttp[G[_]: MonadCancelThrow](
        f: Http[G, F]
    )(implicit kt: KindTransformer[F, G]): Http[G, F] =
      Kleisli { (req: Request[F]) =>
        if (
          !shouldTrace(req.requestPrelude).shouldTrace ||
          !Tracer[F].meta.isEnabled
        ) {
          f(req)
        } else {
          val shared =
            spanAndAttributeProvider.processSharedData(req, routeClassifier, redactor)
          val spanName =
            spanAndAttributeProvider.spanName(req, routeClassifier, redactor, shared)
          val reqAttributes =
            spanAndAttributeProvider.requestAttributes(
              req,
              routeClassifier,
              redactor,
              shared,
              allowedRequestHeaders,
            )
          MonadCancelThrow[G].uncancelable { poll =>
            val tracerG = Tracer[F].mapK[G]
            tracerG.joinOrRoot(req.headers) {
              tracerG
                .spanBuilder(spanName)
                .withSpanKind(SpanKind.Server)
                .addAttributes(reqAttributes)
                .build
                .use { span =>
                  poll(f.run(req))
                    .guaranteeCase {
                      case Outcome.Succeeded(fa) =>
                        fa.flatMap { resp =>
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
                        span.addAttributes(TypedAttributes.errorType(e))
                      case Outcome.Canceled() =>
                        MonadCancelThrow[G].unit
                    }
                }
            }
          }
        }
      }

    /** @return a configured middleware for `HttpApp` */
    def buildHttpApp(f: HttpApp[F]): HttpApp[F] =
      buildGenericTracedHttp(f)

    /** @return a configured middleware for `HttpRoutes` */
    def buildHttpRoutes(f: HttpRoutes[F]): HttpRoutes[F] =
      buildGenericTracedHttp(f)
  }
}
