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

package org.http4s.otel4s.middleware.trace.internal

import cats.effect.kernel.Outcome
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey

private[trace] object TraceAttributes {
  private val ExitCase: AttributeKey[String] = AttributeKey.string("exit.case")

  val Canceled: AttributeKey[Boolean] = AttributeKey.boolean("canceled")
  val Error: AttributeKey[Boolean] = AttributeKey.boolean("error")

  def exitCase[F[_]](outcome: Outcome[F, _, _]): Attribute[String] =
    outcome match {
      case Outcome.Succeeded(_) => ExitCase("succeeded")
      case Outcome.Errored(_) => ExitCase("errored")
      case Outcome.Canceled() => ExitCase("canceled")
    }
}
