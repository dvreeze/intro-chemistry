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

import eu.cdevreeze.introchemistry.periodictable.Element
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.periodictable.PeriodicTable

/**
 * Stoichiometry support, given a periodic table.
 *
 * @author Chris de Vreeze
 */
final class StoichiometrySupport(val periodicTable: PeriodicTable) {

  /**
   * The mass of a formula unit in atomic mass units (or Daltons).
   */
  def massInAmu(formulaUnit: FormulaUnit): BigDecimal = {
    formulaUnit.atomCounts.map { case (elementSymbol: ElementSymbol, atomCount: Int) =>
      val atomMassOfAtom: BigDecimal = massOfAtomInAmu(elementSymbol)
      atomMassOfAtom * atomCount
    }.sum
  }

  /**
   * Returns `massInAmu(formulaUnit)`, interpreted as the mass in grams per mole. That is the beauty of the quantity
   * of a Mole (Avogadro's number as quantity): grams per mole of the formula unit equals the atomic weight (amu) of the formula unit.
   */
  def massInGramPerMole(formulaUnit: FormulaUnit): BigDecimal = {
    massInAmu(formulaUnit)
  }

  /**
   * The mass of an atom in atomic mass units (or Daltons).
   */
  def massOfAtomInAmu(elementSymbol: ElementSymbol): BigDecimal = {
    val element: Element = periodicTable.getElement(elementSymbol)
    val atomMass: BigDecimal = element.atomicMass
    atomMass
  }

  /**
   * Returns `massOfAtomInAmu(elementSymbol)`, interpreted as the mass in grams per mole. That is the beauty of the quantity
   * of a Mole (Avogadro's number as quantity): grams per mole of the atom equals the atomic weight (amu) of the atom.
   */
  def massOfAtomInGramPerMole(elementSymbol: ElementSymbol): BigDecimal = {
    massOfAtomInAmu(elementSymbol)
  }
}

object StoichiometrySupport {

  /**
   * Avogadro's number, which as quanity makes up a mole, like the number 12 makes up a dozen and like number 2 makes up a pair.
   */
  val AvogadrosNumber: BigDecimal = BigDecimal(6.02214076E23)
}
