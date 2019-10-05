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

import eu.cdevreeze.introchemistry.periodictable.load.PeriodicTableLoader
import org.scalatest.funsuite.AnyFunSuite

/**
 * Test for ChemicalEquation balancing. See for example
 * https://chem.libretexts.org/Bookshelves/General_Chemistry/Map%3A_Chemistry_-_The_Central_Science_(Brown_et_al.)/03._Stoichiometry%3A_Calculations_with_Chemical_Formulas_and_Equations/3.1%3A_Chemical_Equations.
 *
 * @author Chris de Vreeze
 */
class ChemicalEquationBalancingTest extends AnyFunSuite {

  private val stoichiometrySupport = StoichiometrySupport(PeriodicTableLoader.newInstance().loadPeriodicTable())

  test("testBalanceSimpleChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("1 Fe (s) + 1 O2 (g) --> 1 Fe2O3 (s)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("4 Fe (s) + 3 O2 (g) --> 2 Fe2O3 (s)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceSimpleIonicChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("1 NaCl (s) --> 1 ion(Na, 1) (aq) + 123 ion(Cl, -1) (aq)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("1 NaCl (s) --> 1 ion(Na, 1) (aq) + 1 ion(Cl, -1) (aq)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceRelativelySimpleChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("1 C6H12O6 + 1 O2 --> 1 CO2 + 1 H2O").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("1 C6H12O6 + 6 O2 --> 6 CO2 + 6 H2O")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceOtherSimpleIonicChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("1 CuCl2 --> 1 ion(Cu, 2) + 1 ion(Cl, -1)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("1 CuCl2 --> 1 ion(Cu, 2) + 2 ion(Cl, -1)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceRelativelySimpleIonicChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("1 Ba(C2H3O2)2 --> 1 ion(Ba, 2) + 1 ion(C2H3O2, -1)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("1 Ba(C2H3O2)2 --> 1 ion(Ba, 2) + 2 ion(C2H3O2, -1)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceComplexChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("1 Ca5(PO4)3OH + 1 H3PO4 + 1 H2O --> 1 Ca(H2PO4)2H2O").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("1 Ca5(PO4)3OH + 7 H3PO4 + 4 H2O --> 5 Ca(H2PO4)2H2O")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceOtherChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("1 Cu(NO3)2 (aq) + 1 K2CO3 (aq) --> 1 Cu(CO3) (s) + 1 K(NO3) (aq)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("1 Cu(NO3)2 (aq) + 1 K2CO3 (aq) --> 1 Cu(CO3) (s) + 2 K(NO3) (aq)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceOtherIonicChemicalEquation") {
    val rawReaction =
      ChemicalEquation(
        "1 ion(Cu, 2) (aq) + 1 ion(NO3, -1) (aq) + 1 ion(K, 1) (aq) + 1 ion(CO3, -2) (aq) --> 1 CuCO3 (s) + 1 ion(K, 1) (aq) + 1 ion(NO3, -1) (aq)")

    val balancedReactionOption: Option[ChemicalEquation] =
      stoichiometrySupport.tryToBalanceChemicalEquation(rawReaction.withoutDuplicates)

    val expectedReaction = ChemicalEquation("1 ion(Cu, 2) (aq) + 1 ion(CO3, -2) (aq) --> 1 CuCO3 (s)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }
}
