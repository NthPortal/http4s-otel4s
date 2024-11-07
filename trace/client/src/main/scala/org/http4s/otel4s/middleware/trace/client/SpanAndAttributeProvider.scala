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
package trace.client

import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

/** Provides a name and attributes for spans using requests and responses.
  *
  * @note Implementations MUST NOT access request or response bodies.
  */
trait SpanAndAttributeProvider extends AttributeProvider { self =>

  /** The type of shared processed data used to provide both the span name and
    * request `Attributes`.
    */
  type Shared

  /** Process data used to provide both the span name and request attributes.
    *
    * @note Implementation MUST NOT access request body.
    */
  def processSharedData[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
  ): Shared

  /** Provides the name for a span using the given request.
    *
    * @note Implementation MUST NOT access request body.
    */
  def spanName[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
      sharedProcessedData: Shared,
  ): String

  /** Provides attributes for a span using the given request.
    *
    * @note Implementation MUST NOT access request body.
    */
  def requestAttributes[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
      sharedProcessedData: Shared,
      allowedHeaders: Set[CIString],
  ): Attributes

  final def requestAttributes[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
      allowedHeaders: Set[CIString],
  ): Attributes =
    requestAttributes(
      request,
      urlTemplateClassifier,
      urlRedactor,
      processSharedData(request, urlTemplateClassifier, urlRedactor),
      allowedHeaders,
    )

  /** Returns an `AttributeProvider` that provides the attributes from this and
    * another `AttributeProvider`.
    *
    * If `that` is a `SpanAndAttributeProvider`, it will not be used to provide
    * span names.
    */
  override def and(that: AttributeProvider): SpanAndAttributeProvider =
    new SpanAndAttributeProvider {
      type Shared = self.Shared

      def processSharedData[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
      ): Shared =
        self.processSharedData(request, urlTemplateClassifier, urlRedactor)

      def spanName[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Shared,
      ): String =
        self.spanName(request, urlTemplateClassifier, urlRedactor, sharedProcessedData)

      def requestAttributes[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Shared,
          allowedHeaders: Set[CIString],
      ): Attributes =
        self.requestAttributes(
          request,
          urlTemplateClassifier,
          urlRedactor,
          sharedProcessedData,
          allowedHeaders,
        ) ++
          that.requestAttributes(
            request,
            urlTemplateClassifier,
            urlRedactor,
            allowedHeaders,
          )

      def responseAttributes[F[_]](
          response: Response[F],
          allowedHeaders: Set[CIString],
      ): Attributes =
        self.responseAttributes(response, allowedHeaders) ++
          that.responseAttributes(response, allowedHeaders)
    }
}

object SpanAndAttributeProvider {

  /** The default provider, which follows OpenTelemetry semantic conventions. */
  def default: SpanAndAttributeProvider = openTelemetry

  /** A `SpanAndAttributeProvider` following OpenTelemetry semantic conventions. */
  val openTelemetry: SpanAndAttributeProvider =
    new SpanAndAttributeProvider {
      // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client

      final case class Data(httpRequestMethod: Attribute[String]) {
        val requestMethodIsUnknown: Boolean =
          httpRequestMethod == TypedAttributes.httpRequestMethodOther
      }

      type Shared = Data

      def processSharedData[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
      ): Data =
        Data(TypedAttributes.httpRequestMethod(request.method))

      def spanName[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Data,
      ): String = {
        val method =
          if (sharedProcessedData.requestMethodIsUnknown) "HTTP"
          else sharedProcessedData.httpRequestMethod.value
        urlTemplateClassifier
          .classify(request.uri)
          .fold(method)(urlTemplate => s"$method $urlTemplate")
      }

      def requestAttributes[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Data,
          allowedHeaders: Set[CIString],
      ): Attributes = {
        val b = Attributes.newBuilder

        b += sharedProcessedData.httpRequestMethod
        b ++= TypedClientAttributes.serverAddress(request.uri.host)
        b ++= TypedClientAttributes.serverPort(request.remotePort, request.uri)
        b += TypedClientAttributes.urlFull(request.uri, urlRedactor)
        // `error.type` handled by `responseAttributes`
        if (sharedProcessedData.requestMethodIsUnknown) {
          b += TypedAttributes.httpRequestMethodOriginal(request.method)
        }
        // `http.response.status_code` handled by `responseAttributes`
        // `network.protocol.name` not required because http4s only supports http
        //   and `Request#httpVersion` is always populated/available
        b ++= TypedClientAttributes.httpRequestResendCount(request)
        request.remote.foreach { socketAddress =>
          b += TypedAttributes.networkPeerAddress(socketAddress.host)
          b += TypedAttributes.networkPeerPort(socketAddress.port)
        }
        b += TypedAttributes.networkProtocolVersion(request.httpVersion)
        TypedAttributes.httpRequestHeadersForBuilder(request.headers, allowedHeaders)(b)
        // `http.response.header.<key>`s handled by `responseAttributes`
        // `network.transport` not opted into at this time
        b ++= TypedAttributes.urlScheme(request.uri.scheme)
        b ++= TypedAttributes.userAgentOriginal(request.headers.get[`User-Agent`])

        b.result()
      }

      def responseAttributes[F[_]](
          response: Response[F],
          allowedHeaders: Set[CIString],
      ): Attributes = {
        val b = Attributes.newBuilder

        if (!response.status.isSuccess) {
          // setting `error.type` for a `Throwable` must be done in the middleware
          b += TypedAttributes.errorType(response.status)
        }
        b += TypedAttributes.httpResponseStatusCode(response.status)
        TypedAttributes.httpResponseHeadersForBuilder(response.headers, allowedHeaders)(b)

        b.result()
      }
    }
}
