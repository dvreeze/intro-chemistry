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
 * Test for Formula creation and parsing and querying.
 *
 * @author Chris de Vreeze
 */
class FormulaTest extends AnyFunSuite {

  import ElementSymbol._

  test("testParseSingleAtomFormula") {
    val frmAsString = "Na"
    val frm = Formula(frmAsString)

    assertResult(true) {
      frm.isSingleElement
    }

    assertResult(Map(Na -> 1)) {
      frm.atomCounts
    }

    assertResult(0) {
      frm.charge
    }

    assertResult(frmAsString) {
      frm.show
    }
  }

  test("testParseElementFormula") {
    val frmAsString = "O2"
    val frm = Formula(frmAsString)

    assertResult(true) {
      frm.isSingleElement
    }

    assertResult(Map(O -> 2)) {
      frm.atomCounts
    }

    assertResult(0) {
      frm.charge
    }

    assertResult(frmAsString) {
      frm.show
    }
  }

  test("testParseSimpleCompoundFormula") {
    val frmAsString = "H2O"
    val frm = Formula(frmAsString)

    assertResult(false) {
      frm.isSingleElement
    }

    assertResult(Map(H -> 2, O -> 1)) {
      frm.atomCounts
    }

    assertResult(0) {
      frm.charge
    }

    assertResult(frmAsString) {
      frm.show
    }
  }

  test("testParseComplexCompoundFormula") {
    val frmAsString = "Ca(H2PO4)2H2O"
    val frm = Formula(frmAsString)

    assertResult(false) {
      frm.isSingleElement
    }

    assertResult(Map(Ca -> 1, H -> 6, P -> 2, O -> 9)) {
      frm.atomCounts
    }

    assertResult(0) {
      frm.charge
    }

    assertResult(frmAsString) {
      frm.show
    }
  }

  test("testParseSingleAtomIonicFormula") {
    val frmAsString = "ion(Na, 1)"
    val frm = Formula(frmAsString)

    assertResult(true) {
      frm.isSingleElement
    }

    assertResult(Map(Na -> 1)) {
      frm.atomCounts
    }

    assertResult(1) {
      frm.charge
    }

    assertResult(frmAsString) {
      frm.show
    }
  }

  test("testParseIonicCompoundFormula") {
    val frmAsString = Formula.Phosphate.show.ensuring(_ == "ion(PO4, -3)")
    val frm = Formula(frmAsString)

    assertResult(false) {
      frm.isSingleElement
    }

    assertResult(Map(P -> 1, O -> 4)) {
      frm.atomCounts
    }

    assertResult(-3) {
      frm.charge
    }

    assertResult(frmAsString) {
      frm.show
    }
  }
}
