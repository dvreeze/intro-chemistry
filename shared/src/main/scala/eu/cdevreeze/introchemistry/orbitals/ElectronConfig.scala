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

import scala.util.Try

import eu.cdevreeze.introchemistry.orbitals.ElectronConfig.SubshellConfig
import eu.cdevreeze.introchemistry.periodictable.ChemicalGroupBlock
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * Electron configuration, either absolute or relative to a previous noble gas in the periodic table.
 *
 * Atomic readii decrease within a period from left to right, because on the one hand the number of protons increases,
 * and on the other hand the added electrons are in the same energy level (so electron shielding decreases). The net result
 * is that electrons are pulled closer to the nucleus, thus reducing the atom radius.
 *
 * Atomic radii increase when going down in the same column in the periodic table, because an energy level is added. So
 * relatively there are more core electrons than valence electrons, thus increasing electron shielding, and therefore
 * increasing the atom radius.
 *
 * When a neutral atom loses electrons and becomes a cation, the atom decreases in size. The reason is that electron-electron
 * repulsion decreases and therefore shielding decreases, resulting in the remaining electrons being pulled closer to the
 * nucleus.
 *
 * When a neutral atom gains electrons and becomes an anion, the atom increases in size. The reason is that electron-electron
 * repulsion and therefore shielding increases, pushing the electrons further apart. Also, the electrons outnumber the protons,
 * thus the extra electrons are not pulled as tightly to the nucleus. Hence the increased atom size.
 *
 * Hence, the lower the charge, the bigger the atom becomes. See for example
 * https://chem.libretexts.org/Bookshelves/Inorganic_Chemistry/Supplemental_Modules_(Inorganic_Chemistry)/Descriptive_Chemistry/Periodic_Trends_of_Elemental_Properties/Periodic_Trends_in_Ionic_Radii.
 *
 * @author Chris de Vreeze
 */
final case class ElectronConfig(previousNobleGasOption: Option[ElementSymbol], subshellConfigs: Seq[SubshellConfig]) {
  require(previousNobleGasOption.forall(_.chemicalGroup == ChemicalGroupBlock.NobleGas), s"Not a noble gas: ${previousNobleGasOption.get}")
  require(previousNobleGasOption.nonEmpty || subshellConfigs.nonEmpty, s"Empty electron config not allowed")

  def isAbsolute: Boolean = previousNobleGasOption.isEmpty

  /**
   * Returns the electron count relative to the optional previous noble gas. If no previous noble gas is given, the absolute
   * electron count is returned.
   */
  def relativeElectronCount: Int = subshellConfigs.map(_.electronCount).sum

  def show: String = {
    val nobleGasString = previousNobleGasOption.map(elm => s"[$elm]").getOrElse("")
    s"$nobleGasString${subshellConfigs.map(_.show).mkString}"
  }

  def minus(subshellConfig: SubshellConfig): ElectronConfig = {
    ElectronConfig(previousNobleGasOption, subshellConfigs.filterNot(Set(subshellConfig)))
  }

  def map(f: SubshellConfig => SubshellConfig): ElectronConfig = {
    ElectronConfig(previousNobleGasOption, subshellConfigs.map(f))
  }
}

object ElectronConfig {

  def parse(s: String): ElectronConfig = {
    val previousNobleGasOption: Option[ElementSymbol] =
      if (s.startsWith("[")) {
        val idx = s.indexOf(']')
        require(idx > 0, s"Syntax error in 'electron config' $s")
        Some(ElementSymbol.parse(s.substring(1, idx)))
      } else {
        None
      }

    val remainder: String = if (s.startsWith("[")) s.substring(s.indexOf("]") + 1) else s
    val subshellConfigs = parseSubshellConfigs(remainder)

    ElectronConfig(previousNobleGasOption, subshellConfigs)
  }

  private def parseSubshellConfigs(s: String): Seq[SubshellConfig] = {
    if (s.isEmpty) {
      Seq.empty
    } else {
      require(s.startsWith("("), s"Syntax error in 'electron config' $s")
      val idx = s.indexOf(")")
      require(idx > 0, s"Syntax error in 'electron config' $s")

      // Recursive call
      parseSubshellConfigs(s.substring(idx + 1)).prepended(SubshellConfig.parse(s.substring(0, idx + 1)))
    }
  }

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

    def minusElectron: SubshellConfig = {
      require(electronCount > 1, s"Electron count is $electronCount, so cannot remove electron")

      this.copy(electronCount = this.electronCount - 1)
    }
  }

  object SubshellConfig {

    def parse(s: String): SubshellConfig = {
      require(s.startsWith("(") && s.endsWith(")") && s.length >= 5, s"Expected format like so: (2p5), but got $s")

      val content = s.drop(1).dropRight(1)
      val level: Int = Try(content.takeWhile(c => Character.isDigit(c)).toInt).getOrElse(sys.error(s"No level found in $s"))
      val contentWithoutLevel = content.dropWhile(c => Character.isDigit(c))
      val subshell = Subshell.parse(contentWithoutLevel.take(1))
      val electronCount = Try(contentWithoutLevel.drop(1).toInt).getOrElse(sys.error(s"No electron count found in $s"))

      SubshellConfig(level, subshell, electronCount)
    }
  }

}
