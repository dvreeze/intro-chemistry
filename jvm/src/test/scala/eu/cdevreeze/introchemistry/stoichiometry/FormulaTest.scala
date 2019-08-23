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

import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import org.scalatest.funsuite.AnyFunSuite

/**
 * Test for Formula creation and parsing and querying.
 *
 * @author Chris de Vreeze
 */
class FormulaTest extends AnyFunSuite {

  import ElementSymbol._
  import Formula._

  test("testParseSimpleFormula") {
    val formulaAsString = "4 Fe + 3 O2 --> 2 Fe2O3"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("Fe"), 4), FormulaUnitQuantity(FormulaUnit("O2"), 3))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("Fe2O3"), 2))) {
      formula.products
    }

    assertResult(true) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseSimpleUnbalancedFormula") {
    val formulaAsString = "4 Fe + 1 O2 --> 2 Fe2O3"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("Fe"), 4), FormulaUnitQuantity(FormulaUnit("O2"), 1))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("Fe2O3"), 2))) {
      formula.products
    }

    assertResult(false) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseSimpleIonicFormula") {
    val formulaAsString = "1 NaCl --> 1 ion(Na, 1) + 1 ion(Cl, -1)"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("NaCl"), 1))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("ion(Na, 1)"), 1), FormulaUnitQuantity(FormulaUnit("ion(Cl, -1)"), 1))) {
      formula.products
    }

    assertResult(true) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseSimpleUnbalancedIonicFormula") {
    val formulaAsString = "1 NaCl --> 1 ion(Na, 1) + 2 ion(Cl, -1)"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("NaCl"), 1))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("ion(Na, 1)"), 1), FormulaUnitQuantity(FormulaUnit("ion(Cl, -1)"), 2))) {
      formula.products
    }

    assertResult(false) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseSimpleUnbalancedWrtChargeIonicFormula") {
    val formulaAsString = "1 NaCl --> 1 ion(Na, 1) + 1 ion(Cl, -3)"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("NaCl"), 1))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("ion(Na, 1)"), 1), FormulaUnitQuantity(FormulaUnit("ion(Cl, -3)"), 1))) {
      formula.products
    }

    assertResult(false) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseRelativelySimpleFormula") {
    val formulaAsString = "1 C6H12O6 + 6 O2 --> 6 CO2 + 6 H2O"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("C6H12O6"), 1), FormulaUnitQuantity(FormulaUnit("O2"), 6))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("CO2"), 6), FormulaUnitQuantity(FormulaUnit("H2O"), 6))) {
      formula.products
    }

    assertResult(true) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseRelativelySimpleUnbalancedFormula") {
    val formulaAsString = "1 C6H12O6 + 6 O2 --> 4 CO2 + 6 H2O"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("C6H12O6"), 1), FormulaUnitQuantity(FormulaUnit("O2"), 6))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("CO2"), 4), FormulaUnitQuantity(FormulaUnit("H2O"), 6))) {
      formula.products
    }

    assertResult(false) {
      formula.isBalanced
    }

    assertResult(true) {
      formula.isBalancedForElement(H)
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseOtherSimpleIonicFormula") {
    val formulaAsString = "1 CuCl2 --> 1 ion(Cu, 2) + 2 ion(Cl, -1)"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("CuCl2"), 1))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("ion(Cu, 2)"), 1), FormulaUnitQuantity(FormulaUnit("ion(Cl, -1)"), 2))) {
      formula.products
    }

    assertResult(true) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseYetAnotherSimpleIonicFormula") {
    val formulaAsString = "1 CuSO4 --> 1 ion(Cu, 2) + 1 ion(SO4, -2)"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("CuSO4"), 1))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("ion(Cu, 2)"), 1), FormulaUnitQuantity(FormulaUnit("ion(SO4, -2)"), 1))) {
      formula.products
    }

    assertResult(true) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseRelativelySimpleIonicFormula") {
    val formulaAsString = "1 Ba(C2H3O2)2 --> 1 ion(Ba, 2) + 2 ion(C2H3O2, -1)"
    val formula = Formula(formulaAsString)

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("Ba(C2H3O2)2"), 1))) {
      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("ion(Ba, 2)"), 1), FormulaUnitQuantity(FormulaUnit("ion(C2H3O2, -1)"), 2))) {
      formula.products
    }

    assertResult(true) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseComplexFormula") {
    val formulaAsString = "1 Ca5(PO4)3OH + 7 H3PO4 + 4 H2O --> 5 Ca(H2PO4)2H2O"
    val formula = Formula(formulaAsString)

    assertResult(Seq(
      FormulaUnitQuantity(FormulaUnit("Ca5(PO4)3OH"), 1),
      FormulaUnitQuantity(FormulaUnit("H3PO4"), 7),
      FormulaUnitQuantity(FormulaUnit("H2O"), 4))) {

      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("Ca(H2PO4)2H2O"), 5))) {
      formula.products
    }

    assertResult(true) {
      formula.isBalanced
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }

  test("testParseComplexUnbalancedFormula") {
    val formulaAsString = "1 Ca5(PO24)3OH + 7 H3PO4 + 4 H2O --> 5 Ca(H2PO4)2H2O"
    val formula = Formula(formulaAsString)

    assertResult(Seq(
      FormulaUnitQuantity(FormulaUnit("Ca5(PO24)3OH"), 1),
      FormulaUnitQuantity(FormulaUnit("H3PO4"), 7),
      FormulaUnitQuantity(FormulaUnit("H2O"), 4))) {

      formula.reactants
    }

    assertResult(Seq(FormulaUnitQuantity(FormulaUnit("Ca(H2PO4)2H2O"), 5))) {
      formula.products
    }

    assertResult(false) {
      formula.isBalanced
    }

    assertResult(true) {
      formula.isBalancedForElement(Ca) && formula.isBalancedForElement(P) && formula.isBalancedForElement(H)
    }

    assertResult(false) {
      formula.isBalancedForElement(O)
    }

    assertResult(formulaAsString) {
      formula.show
    }
  }
}
