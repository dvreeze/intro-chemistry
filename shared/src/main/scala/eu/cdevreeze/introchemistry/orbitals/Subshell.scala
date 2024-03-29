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

package eu.cdevreeze.introchemistry.orbitals

/**
 * Subshell, which is identified by a letter. Note the a subshell (when taken as an absolute value) should also imply
 * the shell (or level, or principal quantum number), but here the subshell is used as a relative notion, relative to
 * the shell (whichever shell it is).
 *
 * @author Chris de Vreeze
 */
sealed trait Subshell {

  def sublevel: Int

  final def name: String = toString.toLowerCase

  final def maxOrbitalCount: Int = 1 + 2 * sublevel

  final def maxElectronCount: Int = 2 * maxOrbitalCount
}

object Subshell {

  case object S extends Subshell {
    def sublevel: Int = 0
  }

  case object P extends Subshell {
    def sublevel: Int = 1
  }

  case object D extends Subshell {
    def sublevel: Int = 2
  }

  case object F extends Subshell {
    def sublevel: Int = 3
  }

  case object G extends Subshell {
    def sublevel: Int = 4
  }

  case object H extends Subshell {
    def sublevel: Int = 5
  }

  val values: Seq[Subshell] = Seq(S, P, D, F, G, H)

  def parse(s: String): Subshell = {
    values.find(_.name == s.toLowerCase).getOrElse(sys.error(s"Not a subshell: $s"))
  }

  def fromSublevel(sublevel: Int): Subshell = {
    values(sublevel)
  }
}
