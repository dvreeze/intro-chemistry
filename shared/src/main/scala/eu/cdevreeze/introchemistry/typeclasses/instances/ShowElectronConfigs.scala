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

package eu.cdevreeze.introchemistry.typeclasses.instances

import eu.cdevreeze.introchemistry.orbitals.ElectronConfig
import eu.cdevreeze.introchemistry.orbitals.ElectronConfig.SubshellConfig
import eu.cdevreeze.introchemistry.typeclasses.Show

/**
 * Holder of instances of type class Show for ElectronConfig etc. Inspired by Cats.
 *
 * @author Chris de Vreeze
 */
object ShowElectronConfigs {

  // Below, only 1 type class instance is an implicit val, for more predictable implicit resolution.

  // Show type class instance for ElectronConfig

  /**
   * Returns a string representation of the electron config. For example: [Xe]6s2 4f14 5d10 6p3.
   */
  implicit val showElectronConfig: Show[ElectronConfig] = Show.show { value: ElectronConfig =>
    val nobleGasString = value.previousNobleGasOption.map(elm => s"[$elm]").getOrElse("")
    s"$nobleGasString${value.subshellConfigs.map(showSubshellConfig.show).mkString(" ")}"
  }

  // Show type class instance for SubshellConfig

  val showSubshellConfig: Show[SubshellConfig] = Show.show { value: SubshellConfig =>
    s"${value.level}${value.subshell.name}${value.electronCount}"
  }

}
