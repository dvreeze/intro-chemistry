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

import org.scalatest.funsuite.AnyFunSuite
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * Test for FormulaUnit creation and parsing and querying.
 *
 * @author Chris de Vreeze
 */
class FormulaUnitTest extends AnyFunSuite {

  import ElementSymbol._

  test("testParseSingleAtomFormulaUnit") {
    val fuAsString = "Na"
    val fu = FormulaUnit(fuAsString)

    assertResult(true) {
      fu.isElement
    }

    assertResult(Map(Na -> 1)) {
      fu.atomCounts
    }

    assertResult(0) {
      fu.charge
    }

    assertResult(fuAsString) {
      fu.show
    }
  }

  test("testParseElementFormulaUnit") {
    val fuAsString = "O2"
    val fu = FormulaUnit(fuAsString)

    assertResult(true) {
      fu.isElement
    }

    assertResult(Map(O -> 2)) {
      fu.atomCounts
    }

    assertResult(0) {
      fu.charge
    }

    assertResult(fuAsString) {
      fu.show
    }
  }

  test("testParseSimpleCompoundFormulaUnit") {
    val fuAsString = "H2O"
    val fu = FormulaUnit(fuAsString)

    assertResult(false) {
      fu.isElement
    }

    assertResult(Map(H -> 2, O -> 1)) {
      fu.atomCounts
    }

    assertResult(0) {
      fu.charge
    }

    assertResult(fuAsString) {
      fu.show
    }
  }

  test("testParseComplexCompoundFormulaUnit") {
    val fuAsString = "Ca(H2PO4)2H2O"
    val fu = FormulaUnit(fuAsString)

    assertResult(false) {
      fu.isElement
    }

    assertResult(Map(Ca -> 1, H -> 6, P -> 2, O -> 9)) {
      fu.atomCounts
    }

    assertResult(0) {
      fu.charge
    }

    assertResult(fuAsString) {
      fu.show
    }
  }

  test("testParseSingleAtomIonicFormulaUnit") {
    val fuAsString = "ion(Na, 1)"
    val fu = FormulaUnit(fuAsString)

    assertResult(true) {
      fu.isElement
    }

    assertResult(Map(Na -> 1)) {
      fu.atomCounts
    }

    assertResult(1) {
      fu.charge
    }

    assertResult(fuAsString) {
      fu.show
    }
  }

  test("testParseIonicCompoundFormulaUnit") {
    val fuAsString = FormulaUnit.Phosphate.show.ensuring(_ == "ion(PO4, -3)")
    val fu = FormulaUnit(fuAsString)

    assertResult(false) {
      fu.isElement
    }

    assertResult(Map(P -> 1, O -> 4)) {
      fu.atomCounts
    }

    assertResult(-3) {
      fu.charge
    }

    assertResult(fuAsString) {
      fu.show
    }
  }
}
