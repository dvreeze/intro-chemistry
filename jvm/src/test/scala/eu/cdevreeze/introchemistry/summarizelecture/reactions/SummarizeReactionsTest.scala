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
  }
}
