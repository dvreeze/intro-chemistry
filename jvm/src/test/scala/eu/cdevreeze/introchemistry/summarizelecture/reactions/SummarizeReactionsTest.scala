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

package eu.cdevreeze.introchemistry.summarizelecture.reactions

import eu.cdevreeze.introchemistry.api.SimpleQueryApi
import eu.cdevreeze.introchemistry.periodictable.load.PeriodicTableLoader
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation
import org.scalatest.funsuite.AnyFunSuite

/**
 * The lecture on reactions summarized, from the Chemistry course given by the University of Kentucky.
 *
 * @author Chris de Vreeze
 */
class SummarizeReactionsTest extends AnyFunSuite {

  private val queryApi = new SimpleQueryApi(PeriodicTableLoader.newInstance().loadPeriodicTable())

  import queryApi._

  test("writingBalancedChemicalEquations") {
    // Reaction of sodium bicarbonate ("baking soda") and acetic acid ("vinegar"), yielding hydrogen oxide (water) and
    // carbon dioxide.

    val ce1 = "1 NaHCO3 + 1 HC2H3O2 --> 1 NaC2H3O2 + 1 H2O + 1 CO2".ce

    assertResult(true) {
      ce1.isBalanced
    }

    // Combustion reaction of methane. Combustion uses oxygen, and yields carbon dioxide and water.

    val rawCe2 = "1 CH4 (g) + 1 O2 (g) --> 1 CO2 (g) + 1 H2O (l)".ce
    val ce2 = tryToBalanceChemicalEquation(rawCe2).get

    assertResult("1 CH4 (g) + 2 O2 (g) --> 1 CO2 (g) + 2 H2O (l)".ce) {
      ce2
    }

    // Reaction of silicon tetrachloride and water, yielding silicon dioxide and hydrochloric acid.

    val rawCe3 = "1 SiCl4 + 1 H2O --> 1 SiO2 + 1 HCl".ce
    val ce3 = tryToBalanceChemicalEquation(rawCe3).get

    assertResult("1 SiCl4 + 2 H2O --> 1 SiO2 + 4 HCl".ce) {
      ce3
    }

    // Reaction of iron and oxygen.

    val rawCe4 = "1 Fe + 1 O2 --> 1 Fe2O3".ce
    val ce4 = tryToBalanceChemicalEquation(rawCe4).get

    assertResult("4 Fe + 3 O2 --> 2 Fe2O3".ce) {
      ce4
    }
  }

  test("writingBalancedChemicalEquationsExercise") {
    val rawEquations: Seq[ChemicalEquation] = Seq(
      "1 NaBr + 1 F2 --> 1 NaF + 1 Br2",
      "1 K + 1 H2O --> 1 KOH + 1 H2",
      "1 H2O2 --> 1 H2O + 1 O2",
      "1 CuSO4 + 1 KCN --> 1 Cu(CN)2 + 1 K2SO4",
      "1 P4 + 1 O2 --> 1 P4O6",
      "1 CH4 + 1 O2 --> 1 CO2 + 1 H2O",
      "1 N2 + 1 F2 --> 1 NF3",
      "1 AlBr3 + 1 K2SO4 --> 1 KBr + 1 Al2(SO4)3",
    ).map(s => s.ce)

    val expectedEquations: Seq[ChemicalEquation] = Seq(
      "2 NaBr + 1 F2 --> 2 NaF + 1 Br2",
      "2 K + 2 H2O --> 2 KOH + 1 H2",
      "2 H2O2 --> 2 H2O + 1 O2",
      "1 CuSO4 + 2 KCN --> 1 Cu(CN)2 + 1 K2SO4",
      "1 P4 + 3 O2 --> 1 P4O6",
      "1 CH4 + 2 O2 --> 1 CO2 + 2 H2O",
      "1 N2 + 3 F2 --> 2 NF3",
      "2 AlBr3 + 3 K2SO4 --> 6 KBr + 1 Al2(SO4)3",
    ).map(s => s.ce.ensuring(ce => ce.isBalanced))

    assertResult(expectedEquations) {
      rawEquations.map(ce => tryToBalanceChemicalEquation(ce).get)
    }
  }
}
