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

package eu.cdevreeze.introchemistry.api

import scala.annotation.tailrec
import scala.util.chaining._

import eu.cdevreeze.introchemistry.orbitals.ElectronConfig
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.periodictable.PeriodicTable
import eu.cdevreeze.introchemistry.stoichiometry.Formula

/**
 * Simple "orbitals" query API, to be mixed in into SimpleQueryApi.
 *
 * @author Chris de Vreeze
 */
trait SimpleOrbitalsQueryApi {

  def periodicTable: PeriodicTable

  // Aufbau-principle-related queries. In this case the periodic table is consulted (unlike in object AufbauPrinciple).

  final def getElectronConfigByAtomicNumber(atomicNumber: Int): ElectronConfig = {
    ElectronConfig.parse(periodicTable.getElementByAtomicNumber(atomicNumber).electronConfiguration)
  }

  final def getElectronConfig(element: ElementSymbol): ElectronConfig = {
    getElectronConfigByAtomicNumber(element.atomicNumber)
  }

  final def getAbsoluteElectronConfig(element: ElementSymbol): ElectronConfig = {
    val rawResult = getElectronConfig(element)

    if (rawResult.previousNobleGasOption.isEmpty) {
      rawResult
    } else {
      // Recursive call
      val prevResult: ElectronConfig =
        getAbsoluteElectronConfig(rawResult.previousNobleGasOption.get).ensuring(_.previousNobleGasOption.isEmpty)

      ElectronConfig(None, prevResult.subshellConfigs.concat(rawResult.subshellConfigs))
    }
  }

  final def getElectronConfigOfIon(element: ElementSymbol, charge: Int): ElectronConfig = {
    require(charge != 0, s"Not an ion: $element with charge 0")

    if (charge < 0) {
      getElectronConfigByAtomicNumber(element.atomicNumber + charge.abs)
    } else {
      assert(charge > 0)

      1.to(charge).foldLeft(getElectronConfig(element)) { case (accElectronConfig, _) =>
        removeElectron(accElectronConfig)
      }.pipe(cfg => if (cfg.subshellConfigs.isEmpty) getElectronConfig(cfg.previousNobleGasOption.get) else cfg)
    }
  }

  final def getAbsoluteElectronConfigOfIon(element: ElementSymbol, charge: Int): ElectronConfig = {
    getAbsoluteElectronConfig(getElectronConfigOfIon(element, charge))
  }

  final def getAbsoluteElectronConfig(electronConfig: ElectronConfig): ElectronConfig = {
    if (electronConfig.previousNobleGasOption.isEmpty) {
      electronConfig
    } else {
      // Calling overloaded getAbsoluteElectronConfig function, passing an element symbol
      val prevResult: ElectronConfig =
        getAbsoluteElectronConfig(electronConfig.previousNobleGasOption.get).ensuring(_.previousNobleGasOption.isEmpty)

      ElectronConfig(None, prevResult.subshellConfigs.concat(electronConfig.subshellConfigs))
    }
  }

  final def getElectronCount(electronConfig: ElectronConfig): Int = {
    getAbsoluteElectronConfig(electronConfig).ensuring(_.previousNobleGasOption.isEmpty).relativeElectronCount
  }

  /**
   * Returns the electron count of the (atomic) element. This must be equal to the proton count, or atomic number, of course.
   */
  final def getElectronCount(element: ElementSymbol): Int = {
    getElectronCount(getElectronConfig(element))
  }

  /**
   * Returns the number of valence electrons, as needed for drawing Lewis structure diagrams.
   */
  final def getValenceElectronCount(electronConfig: ElectronConfig): Int = {
    val absoluteElectronConfig = getAbsoluteElectronConfig(electronConfig).ensuring(_.previousNobleGasOption.isEmpty)

    val maxLevelOption: Option[Int] = absoluteElectronConfig.subshellConfigs.map(_.level).maxOption
    absoluteElectronConfig.subshellConfigs.filter(c => maxLevelOption.contains(c.level)).map(_.electronCount).sum
  }

  /**
   * Returns the number of valence electrons of the given element, as needed for drawing Lewis structure diagrams.
   */
  final def getValenceElectronCount(element: ElementSymbol): Int = {
    getValenceElectronCount(getElectronConfig(element))
  }

  /**
   * Returns the number of valence electrons of the given formula, as needed for drawing Lewis structure diagrams.
   */
  final def getValenceElectronCount(formula: Formula): Int = {
    val resultIgnoringCharge =
      formula.atomCounts.map { case (elem: ElementSymbol, count: Int) => count * getValenceElectronCount(elem) }.sum

    resultIgnoringCharge - formula.charge
  }

  @tailrec
  final def removeElectron(electronConfig: ElectronConfig): ElectronConfig = {
    if (electronConfig.subshellConfigs.isEmpty) {
      assert(electronConfig.previousNobleGasOption.nonEmpty)

      // Recursive call
      removeElectron(
        getElectronConfig(electronConfig.previousNobleGasOption.get).ensuring(_.subshellConfigs.nonEmpty))
    } else {
      val maxLevel: Int = electronConfig.subshellConfigs.map(_.level).max
      val targetSubshellConfig: ElectronConfig.SubshellConfig =
        electronConfig.subshellConfigs.filter(_.level == maxLevel).sortBy(_.subshell.sublevel).last

      if (targetSubshellConfig.electronCount == 1) {
        electronConfig.minus(targetSubshellConfig)
      } else {
        electronConfig.map(cfg => if (cfg == targetSubshellConfig) cfg.minusElectron else cfg)
      }
    }
  }
}
