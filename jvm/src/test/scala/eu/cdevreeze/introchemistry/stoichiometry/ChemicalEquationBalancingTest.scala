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
    val unbalancedReaction = ChemicalEquation("Fe (s) + O2 (g) = Fe2O3 (s)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("4 Fe (s) + 3 O2 (g) = 2 Fe2O3 (s)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceSimpleIonicChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("NaCl (s) = Na{1} (aq) + 123 Cl{-1} (aq)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("NaCl (s) = Na{1} (aq) + Cl{-1} (aq)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceRelativelySimpleChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("C6H12O6 + O2 = CO2 + H2O").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("C6H12O6 + 6 O2 = 6 CO2 + 6 H2O")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceOtherSimpleIonicChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("CuCl2 = Cu{2} + Cl{-1}").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("CuCl2 = Cu{2} + 2 Cl{-1}")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceRelativelySimpleIonicChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("Ba(C2H3O2)2 = Ba{2} + C2H3O2{-1}").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("Ba(C2H3O2)2 = Ba{2} + 2 C2H3O2{-1}")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceComplexChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("Ca5(PO4)3OH + H3PO4 + H2O = Ca(H2PO4)2H2O").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("Ca5(PO4)3OH + 7 H3PO4 + 4 H2O = 5 Ca(H2PO4)2H2O")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceOtherChemicalEquation") {
    val unbalancedReaction = ChemicalEquation("Cu(NO3)2 (aq) + K2CO3 (aq) = Cu(CO3) (s) + K(NO3) (aq)").ensuring(!_.isBalanced)

    val balancedReactionOption: Option[ChemicalEquation] = stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction)

    val expectedReaction = ChemicalEquation("Cu(NO3)2 (aq) + K2CO3 (aq) = Cu(CO3) (s) + 2 K(NO3) (aq)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  test("testBalanceOtherIonicChemicalEquation") {
    val rawReaction =
      ChemicalEquation(
        "Cu{2} (aq) + NO3{-1} (aq) + K{1} (aq) + CO3{-2} (aq) = CuCO3 (s) + K{1} (aq) + NO3{-1} (aq)")

    val balancedReactionOption: Option[ChemicalEquation] =
      stoichiometrySupport.tryToBalanceChemicalEquation(rawReaction.withoutDuplicates)

    val expectedReaction = ChemicalEquation("Cu{2} (aq) + CO3{-2} (aq) = CuCO3 (s)")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }

  /*
  test("testBalanceLongChemicalEquation") {
    val unbalancedReaction =
      ChemicalEquation("K4Fe(CN)6 + KMnO4 + H2SO4 = KHSO4 + Fe2(SO4)3 + MnSO4 + HNO3 + CO2 + H2O").ensuring(!_.isBalanced)

    val colValues: SeqMap[Int, Long] = SeqMap(0 -> 10L, 1 -> 122L, 8 -> 188L)

    val balancedReactionOption: Option[ChemicalEquation] =
      stoichiometrySupport.tryToBalanceChemicalEquation(unbalancedReaction, colValues)

    val expectedReaction =
      ChemicalEquation("10 K4Fe(CN)6 + 122 KMnO4 + 299 H2SO4 = 162 KHSO4 + 5 Fe2(SO4)3 + 122 MnSO4 + 60 HNO3 + 60 CO2 + 188 H2O")

    assertResult(Some(expectedReaction)) {
      balancedReactionOption
    }
  }
  */
}
