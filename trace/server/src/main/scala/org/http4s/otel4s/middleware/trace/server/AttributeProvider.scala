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
package trace.server

import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attributes

/** Provides attributes for spans using requests and responses.
  *
  * @note Implementations MUST NOT access request or response bodies.
  */
trait AttributeProvider { self =>

  /** Provides attributes for a span using the given request.
    *
    * @note Implementation MUST NOT access request body.
    */
  def requestAttributes[F[_]](
      request: Request[F],
      routeClassifier: RouteClassifier,
      redactor: PathAndQueryRedactor,
      allowedHeaders: Set[CIString],
  ): Attributes

  /** Provides attributes for a span using the given response.
    *
    * @note Implementation MUST NOT access response body.
    */
  def responseAttributes[F[_]](
      response: Response[F],
      allowedHeaders: Set[CIString],
  ): Attributes

  /** @return an `AttributeProvider` that provides the attributes from this and
    *         another `AttributeProvider`
    */
  def and(that: AttributeProvider): AttributeProvider =
    new AttributeProvider {
      def requestAttributes[F[_]](
          request: Request[F],
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
          allowedHeaders: Set[CIString],
      ): Attributes =
        self.requestAttributes(
          request,
          routeClassifier,
          redactor,
          allowedHeaders,
        ) ++
          that.requestAttributes(
            request,
            routeClassifier,
            redactor,
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
