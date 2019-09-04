/*
 * Copyright 2019-2019 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.introchemistry.stoichiometry

/**
 * Phase of a species: solid, liquid, gas or aqueous.
 *
 * @author Chris de Vreeze
 */
sealed trait Phase

object Phase {

  case object Solid extends Phase {
    override def toString: String = "s"
  }

  case object Liquid extends Phase {
    override def toString: String = "l"
  }

  case object Gas extends Phase {
    override def toString: String = "g"
  }

  case object Aqueous extends Phase {
    override def toString: String = "aq"
  }

  def apply(s: String): Phase = Phase.parse(s)

  def parse(s: String): Phase = {
    s match {
      case "s" => Solid
      case "l" => Liquid
      case "g" => Gas
      case "aq" => Aqueous
      case _ => sys.error(s"Not a phase: $s")
    }
  }
}
