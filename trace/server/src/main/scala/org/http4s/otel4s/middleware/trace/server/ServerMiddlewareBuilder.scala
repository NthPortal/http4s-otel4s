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
import cats.syntax.functor._
import fs2.Stream
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.TracerProvider

/** Middleware builder for wrapping an http4s `Server` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
  */
class ServerMiddlewareBuilder[F[_]: TracerProvider: MonadCancelThrow] private (
    redactor: PathAndQueryRedactor, // cannot safely have default value
    spanDataProvider: SpanDataProvider,
    routeClassifier: RouteClassifier,
    headersAllowedAsAttributes: HeadersAllowedAsAttributes,
    shouldTrace: RequestPrelude => ShouldTrace,
) {
  private def copy(
      spanDataProvider: SpanDataProvider = this.spanDataProvider,
      routeClassifier: RouteClassifier = this.routeClassifier,
      headersAllowedAsAttributes: HeadersAllowedAsAttributes = this.headersAllowedAsAttributes,
      shouldTrace: RequestPrelude => ShouldTrace = this.shouldTrace,
  ): ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      this.redactor,
      spanDataProvider,
      routeClassifier,
      headersAllowedAsAttributes,
      shouldTrace,
    )

  /** Sets how a span's name and `Attributes` are derived from a request and
    * response.
    */
  def withSpanDataProvider(spanDataProvider: SpanDataProvider): ServerMiddlewareBuilder[F] =
    copy(spanDataProvider = spanDataProvider)

  /** Sets how to determine the route within the application from a request. */
  def withRouteClassifier(routeClassifier: RouteClassifier): ServerMiddlewareBuilder[F] =
    copy(routeClassifier = routeClassifier)

  /** Sets which headers are allowed to be made into `Attribute`s. */
  def withHeadersAllowedAsAttributes(
      headersAllowedAsAttributes: HeadersAllowedAsAttributes
  ): ServerMiddlewareBuilder[F] =
    copy(headersAllowedAsAttributes = headersAllowedAsAttributes)

  /** Sets how to determine when to trace a request and its response. */
  def withShouldTrace(shouldTrace: RequestPrelude => ShouldTrace): ServerMiddlewareBuilder[F] =
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
  )(implicit kt: KindTransformer[F, G]): F[Http[G, F]] =
    for {
      tracerF <- TracerProvider[F]
        .tracer("org.http4s.otel4s.middleware.server")
        .withVersion(org.http4s.otel4s.middleware.BuildInfo.version)
        .get
    } yield Kleisli { (req: Request[F]) =>
      if (
        !shouldTrace(req.requestPrelude).shouldTrace ||
        !tracerF.meta.isEnabled
      ) {
        f(req)
      } else {
        val reqNoBody = req.withBodyStream(Stream.empty)
        val shared =
          spanDataProvider.processSharedData(reqNoBody, routeClassifier, redactor)
        val spanName =
          spanDataProvider.spanName(reqNoBody, routeClassifier, redactor, shared)
        val reqAttributes =
          spanDataProvider.requestAttributes(
            reqNoBody,
            routeClassifier,
            redactor,
            shared,
            headersAllowedAsAttributes.request,
          )
        MonadCancelThrow[G].uncancelable { poll =>
          val tracerG = tracerF.mapK[G]
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
                          spanDataProvider.responseAttributes(
                            resp.withBodyStream(Stream.empty),
                            headersAllowedAsAttributes.response,
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
  def buildHttpApp(f: HttpApp[F]): F[HttpApp[F]] =
    buildGenericTracedHttp(f)

  /** @return a configured middleware for `HttpRoutes` */
  def buildHttpRoutes(f: HttpRoutes[F]): F[HttpRoutes[F]] =
    buildGenericTracedHttp(f)
}

object ServerMiddlewareBuilder {

  /** @return a server middleware builder with default configuration */
  def default[F[_]: TracerProvider: MonadCancelThrow](
      redactor: PathAndQueryRedactor
  ): ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      redactor,
      Defaults.spanDataProvider,
      Defaults.routeClassifier,
      Defaults.headersAllowedAsAttributes,
      Defaults.shouldTrace,
    )

  /** The default configuration values for a server middleware builder. */
  object Defaults {
    def spanDataProvider: SpanDataProvider = SpanDataProvider.default
    val routeClassifier: RouteClassifier = RouteClassifier.indeterminate
    def headersAllowedAsAttributes: HeadersAllowedAsAttributes =
      HeadersAllowedAsAttributes.default
    val shouldTrace: RequestPrelude => ShouldTrace = _ => ShouldTrace.Trace
  }
}
