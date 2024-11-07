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

import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Port
import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.attributes.UserAgentAttributes

import java.util.Locale

/** Methods for creating appropriate `Attribute`s from typed HTTP objects. */
object TypedAttributes {
  private[this] lazy val knownMethods: Set[Method] = Method.all.toSet

  /** The http.request.method `Attribute` with the special value _OTHER */
  val httpRequestMethodOther: Attribute[String] =
    HttpAttributes.HttpRequestMethod("_OTHER")

  /** @return the `error.type` `Attribute` */
  def errorType(cause: Throwable): Attribute[String] =
    ErrorAttributes.ErrorType(cause.getClass.getName)

  /** @return the `error.type` `Attribute` */
  def errorType(status: Status): Attribute[String] =
    ErrorAttributes.ErrorType(s"${status.code}")

  /** @return the `http.request.method` `Attribute` */
  def httpRequestMethod(method: Method): Attribute[String] =
    if (knownMethods.contains(method)) HttpAttributes.HttpRequestMethod(method.name)
    else httpRequestMethodOther

  /** @return the `http.request.method_original` `Attribute` */
  def httpRequestMethodOriginal(method: Method): Attribute[String] =
    HttpAttributes.HttpRequestMethodOriginal(method.name)

  /** @return the `http.response.status_code` `Attribute` */
  def httpResponseStatusCode(status: Status): Attribute[Long] =
    HttpAttributes.HttpResponseStatusCode(status.code.toLong)

  /** @return the `network.peer.address` `Attribute` */
  def networkPeerAddress(ip: IpAddress): Attribute[String] =
    NetworkAttributes.NetworkPeerAddress(ip.toString)

  /** @return the `network.peer.port` `Attribute` */
  def networkPeerPort(port: Port): Attribute[Long] =
    NetworkAttributes.NetworkPeerPort(port.value.toLong)

  /** @return the `network.protocol.version` `Attribute` */
  def networkProtocolVersion(version: HttpVersion): Attribute[String] = {
    val rendered = version.major match {
      case m if m <= 1 => s"$m.${version.minor}"
      case m /* if m >= 2 */ => s"$m"
    }
    NetworkAttributes.NetworkProtocolVersion(rendered)
  }

  /** @return the `url.scheme` `Attribute` */
  def urlScheme(scheme: Uri.Scheme): Attribute[String] =
    UrlAttributes.UrlScheme(scheme.value)

  /** @return the `url.scheme` `Attribute` */
  def urlScheme(scheme: Option[Uri.Scheme]): Option[Attribute[String]] =
    scheme.map(urlScheme)

  /** @return the `user_agent.original` `Attribute` */
  def userAgentOriginal(userAgent: `User-Agent`): Attribute[String] =
    UserAgentAttributes.UserAgentOriginal(`User-Agent`.headerInstance.value(userAgent))

  /** @return the `user_agent.original` `Attribute` */
  def userAgentOriginal(userAgent: Option[`User-Agent`]): Option[Attribute[String]] =
    userAgent.map(userAgentOriginal)

  /* header stuff here, because it's long */

  /** Methods for creating appropriate `Attribute`s from typed HTTP headers. */
  private[this] def genericHttpHeaders(
      headers: Headers,
      allowedHeaders: Set[CIString],
      prefixKey: AttributeKey[Seq[String]],
  )(b: Attributes.Builder): b.type =
    b ++= headers
      .redactSensitive()
      .headers
      .groupMap(_.name)(_.value)
      .view
      .collect {
        case (name, values) if allowedHeaders.contains(name) =>
          val key =
            prefixKey
              .transformName(_ + "." + name.toString.toLowerCase(Locale.ROOT))
          Attribute(key, values)
      }

  /** Adds the `http.request.header.<lowercase name>` `Attribute`s for all
    * headers in `allowedHeaders` to the provided builder.
    */
  def httpRequestHeadersForBuilder(headers: Headers, allowedHeaders: Set[CIString])(
      b: Attributes.Builder
  ): b.type =
    if (allowedHeaders.isEmpty) b
    else genericHttpHeaders(headers, allowedHeaders, HttpAttributes.HttpRequestHeader)(b)

  /** @return `http.request.header.<lowercase name>` `Attributes` for
    *          all headers in `allowedHeaders`
    */
  def httpRequestHeaders(headers: Headers, allowedHeaders: Set[CIString]): Attributes =
    if (allowedHeaders.isEmpty) Attributes.empty
    else httpRequestHeadersForBuilder(headers, allowedHeaders)(Attributes.newBuilder).result()

  /** Adds the `http.response.header.<lowercase name>` `Attribute`s for all
    * headers in `allowedHeaders` to the provided builder.
    */
  def httpResponseHeadersForBuilder(headers: Headers, allowedHeaders: Set[CIString])(
      b: Attributes.Builder
  ): b.type =
    if (allowedHeaders.isEmpty) b
    else genericHttpHeaders(headers, allowedHeaders, HttpAttributes.HttpResponseHeader)(b)

  /** @return `http.response.header.<lowercase name>` `Attribute`s for
    *          all headers in `allowedHeaders`
    */
  def httpResponseHeaders(headers: Headers, allowedHeaders: Set[CIString]): Attributes =
    if (allowedHeaders.isEmpty) Attributes.empty
    else httpRequestHeadersForBuilder(headers, allowedHeaders)(Attributes.newBuilder).result()

  /** The default set of HTTP headers allowed to be turned into `Attribute`s. */
  lazy val defaultAllowedHttpHeaders: Set[CIString] = Set(
    "Accept",
    "Accept-CH",
    "Accept-Charset",
    "Accept-CH-Lifetime",
    "Accept-Encoding",
    "Accept-Language",
    "Accept-Ranges",
    "Access-Control-Allow-Credentials",
    "Access-Control-Allow-Headers",
    "Access-Control-Allow-Origin",
    "Access-Control-Expose-Methods",
    "Access-Control-Max-Age",
    "Access-Control-Request-Headers",
    "Access-Control-Request-Method",
    "Age",
    "Allow",
    "Alt-Svc",
    "B3",
    "Cache-Control",
    "Clear-Site-Data",
    "Connection",
    "Content-Disposition",
    "Content-Encoding",
    "Content-Language",
    "Content-Length",
    "Content-Location",
    "Content-Range",
    "Content-Security-Policy",
    "Content-Security-Policy-Report-Only",
    "Content-Type",
    "Cross-Origin-Embedder-Policy",
    "Cross-Origin-Opener-Policy",
    "Cross-Origin-Resource-Policy",
    "Date",
    "Deprecation",
    "Device-Memory",
    "DNT",
    "Early-Data",
    "ETag",
    "Expect",
    "Expect-CT",
    "Expires",
    "Feature-Policy",
    "Forwarded",
    "From",
    "Host",
    "If-Match",
    "If-Modified-Since",
    "If-None-Match",
    "If-Range",
    "If-Unmodified-Since",
    "Keep-Alive",
    "Large-Allocation",
    "Last-Modified",
    "Link",
    "Location",
    "Max-Forwards",
    "Origin",
    "Pragma",
    "Proxy-Authenticate",
    "Public-Key-Pins",
    "Public-Key-Pins-Report-Only",
    "Range",
    "Referer",
    "Referer-Policy",
    "Retry-After",
    "Save-Data",
    "Sec-CH-UA",
    "Sec-CH-UA-Arch",
    "Sec-CH-UA-Bitness",
    "Sec-CH-UA-Full-Version",
    "Sec-CH-UA-Full-Version-List",
    "Sec-CH-UA-Mobile",
    "Sec-CH-UA-Model",
    "Sec-CH-UA-Platform",
    "Sec-CH-UA-Platform-Version",
    "Sec-Fetch-Dest",
    "Sec-Fetch-Mode",
    "Sec-Fetch-Site",
    "Sec-Fetch-User",
    "Server",
    "Server-Timing",
    "SourceMap",
    "Strict-Transport-Security",
    "TE",
    "Timing-Allow-Origin",
    "Tk",
    "Trailer",
    "Transfer-Encoding",
    "Upgrade",
    "User-Agent",
    "Vary",
    "Via",
    "Viewport-Width",
    "Warning",
    "Width",
    "WWW-Authenticate",
    "X-B3-Sampled",
    "X-B3-SpanId",
    "X-B3-TraceId",
    "X-Content-Type-Options",
    "X-DNS-Prefetch-Control",
    "X-Download-Options",
    "X-Forwarded-For",
    "X-Forwarded-Host",
    "X-Forwarded-Port",
    "X-Forwarded-Proto",
    "X-Forwarded-Scheme",
    "X-Frame-Options",
    "X-Permitted-Cross-Domain-Policies",
    "X-Powered-By",
    "X-Real-Ip",
    "X-Request-Id",
    "X-Request-Start",
    "X-Runtime",
    "X-Scheme",
    "X-SourceMap",
    "X-XSS-Protection",
  ).map(CIString(_))

}
