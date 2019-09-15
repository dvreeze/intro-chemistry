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

import eu.cdevreeze.introchemistry.orbitals.ElectronConfig.SubshellConfig
import eu.cdevreeze.introchemistry.periodictable.ChemicalGroupBlock
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * Electron configuration, either absolute or relative to a previous noble gas in the periodic table.
 *
 * @author Chris de Vreeze
 */
final case class ElectronConfig(previousNobleGasOption: Option[ElementSymbol], subshellConfigs: Seq[SubshellConfig]) {
  require(previousNobleGasOption.forall(_.chemicalGroup == ChemicalGroupBlock.NobleGas), s"Not a noble gas: ${previousNobleGasOption.get}")

  def isAbsolute: Boolean = previousNobleGasOption.isEmpty

  def relativeElectronCount: Int = subshellConfigs.map(_.electronCount).sum

  def show: String = {
    val nobleGasString = previousNobleGasOption.map(elm => s"[$elm]").getOrElse("")
    s"$nobleGasString${subshellConfigs.map(_.show).mkString}"
  }
}

object ElectronConfig {

  /**
   * Subshell configuration. It contains a level, subshell and electron count. The level, or shell, corresponds to the
   * principal quantum number. The subshell corresponds to the angular momentum quantum number.
   */
  final case class SubshellConfig(level: Int, subshell: Subshell, electronCount: Int) {
    require(
      level > 0,
      s"The level (principal quantum number) must be at least 1, but got $level instead")

    require(
      subshell.sublevel >= 0,
      s"The subshell (or sublevel, or angular momentum quantum number) must be at least 0, but got ${subshell.sublevel} instead")
    require(
      subshell.sublevel < level,
      s"The subshell (or sublevel, or angular momentum quantum number) must be at most ${level - 1}, " +
        s"but got ${subshell.sublevel} instead")

    require(electronCount > 0, s"The electron count must be greater than zero, but got $electronCount instead")
    require(
      electronCount <= subshell.maxElectronCount,
      s"The electron count must be at most ${subshell.maxElectronCount}, but got $electronCount instead")

    def show: String = s"($level${subshell.name}$electronCount)"
  }
}
