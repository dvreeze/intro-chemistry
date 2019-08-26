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
 * Test for ChemicalEquation creation and parsing and querying.
 *
 * @author Chris de Vreeze
 */
class ChemicalEquationTest extends AnyFunSuite {

  import ElementSymbol._
  import ChemicalEquation._

  test("testParseSimpleChemicalEquation") {
    val reactionAsString = "4 Fe + 3 O2 --> 2 Fe2O3"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("Fe"), 4), FormulaQuantity(Formula("O2"), 3))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Fe2O3"), 2))) {
      reaction.products
    }

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseSimpleUnbalancedChemicalEquation") {
    val reactionAsString = "4 Fe + 1 O2 --> 2 Fe2O3"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("Fe"), 4), FormulaQuantity(Formula("O2"), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Fe2O3"), 2))) {
      reaction.products
    }

    assertResult(false) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseSimpleIonicChemicalEquation") {
    val reactionAsString = "1 NaCl --> 1 ion(Na, 1) + 1 ion(Cl, -1)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("NaCl"), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("ion(Na, 1)"), 1), FormulaQuantity(Formula("ion(Cl, -1)"), 1))) {
      reaction.products
    }

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseSimpleUnbalancedIonicChemicalEquation") {
    val reactionAsString = "1 NaCl --> 1 ion(Na, 1) + 2 ion(Cl, -1)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("NaCl"), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("ion(Na, 1)"), 1), FormulaQuantity(Formula("ion(Cl, -1)"), 2))) {
      reaction.products
    }

    assertResult(false) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseSimpleUnbalancedWrtChargeIonicChemicalEquation") {
    val reactionAsString = "1 NaCl --> 1 ion(Na, 1) + 1 ion(Cl, -3)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("NaCl"), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("ion(Na, 1)"), 1), FormulaQuantity(Formula("ion(Cl, -3)"), 1))) {
      reaction.products
    }

    assertResult(false) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseRelativelySimpleChemicalEquation") {
    val reactionAsString = "1 C6H12O6 + 6 O2 --> 6 CO2 + 6 H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("C6H12O6"), 1), FormulaQuantity(Formula("O2"), 6))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("CO2"), 6), FormulaQuantity(Formula("H2O"), 6))) {
      reaction.products
    }

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseRelativelySimpleUnbalancedChemicalEquation") {
    val reactionAsString = "1 C6H12O6 + 6 O2 --> 4 CO2 + 6 H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("C6H12O6"), 1), FormulaQuantity(Formula("O2"), 6))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("CO2"), 4), FormulaQuantity(Formula("H2O"), 6))) {
      reaction.products
    }

    assertResult(false) {
      reaction.isBalanced
    }

    assertResult(true) {
      reaction.isBalancedForElement(H)
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseOtherSimpleIonicChemicalEquation") {
    val reactionAsString = "1 CuCl2 --> 1 ion(Cu, 2) + 2 ion(Cl, -1)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("CuCl2"), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("ion(Cu, 2)"), 1), FormulaQuantity(Formula("ion(Cl, -1)"), 2))) {
      reaction.products
    }

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseYetAnotherSimpleIonicChemicalEquation") {
    val reactionAsString = "1 CuSO4 --> 1 ion(Cu, 2) + 1 ion(SO4, -2)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("CuSO4"), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("ion(Cu, 2)"), 1), FormulaQuantity(Formula("ion(SO4, -2)"), 1))) {
      reaction.products
    }

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseRelativelySimpleIonicChemicalEquation") {
    val reactionAsString = "1 Ba(C2H3O2)2 --> 1 ion(Ba, 2) + 2 ion(C2H3O2, -1)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("Ba(C2H3O2)2"), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("ion(Ba, 2)"), 1), FormulaQuantity(Formula("ion(C2H3O2, -1)"), 2))) {
      reaction.products
    }

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseComplexChemicalEquation") {
    val reactionAsString = "1 Ca5(PO4)3OH + 7 H3PO4 + 4 H2O --> 5 Ca(H2PO4)2H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(
      FormulaQuantity(Formula("Ca5(PO4)3OH"), 1),
      FormulaQuantity(Formula("H3PO4"), 7),
      FormulaQuantity(Formula("H2O"), 4))) {

      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Ca(H2PO4)2H2O"), 5))) {
      reaction.products
    }

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }

  test("testParseComplexUnbalancedChemicalEquation") {
    val reactionAsString = "1 Ca5(PO24)3OH + 7 H3PO4 + 4 H2O --> 5 Ca(H2PO4)2H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(
      FormulaQuantity(Formula("Ca5(PO24)3OH"), 1),
      FormulaQuantity(Formula("H3PO4"), 7),
      FormulaQuantity(Formula("H2O"), 4))) {

      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Ca(H2PO4)2H2O"), 5))) {
      reaction.products
    }

    assertResult(false) {
      reaction.isBalanced
    }

    assertResult(true) {
      reaction.isBalancedForElement(Ca) && reaction.isBalancedForElement(P) && reaction.isBalancedForElement(H)
    }

    assertResult(false) {
      reaction.isBalancedForElement(O)
    }

    assertResult(reactionAsString) {
      reaction.show
    }
  }
}