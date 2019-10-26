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
import eu.cdevreeze.introchemistry.typeclasses.instances.ShowChemicalEquations._
import eu.cdevreeze.introchemistry.typeclasses.show.ShowSyntax._
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
    val reactionAsString = "4 Fe (s) + 3 O2 (g) = 2 Fe2O3 (s)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("Fe"), Some(Phase.Solid), 4), FormulaQuantity(Formula("O2"), Some(Phase.Gas), 3))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Fe2O3"), Some(Phase.Solid), 2))) {
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
    val reactionAsString = "4 Fe (s) + O2 (g) = 2 Fe2O3 (s)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("Fe"), Some(Phase.Solid), 4), FormulaQuantity(Formula("O2"), Some(Phase.Gas), 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Fe2O3"), Some(Phase.Solid), 2))) {
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
    val reactionAsString = "NaCl (s) = Na{1} (aq) + Cl{-1} (aq)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("NaCl"), Some(Phase.Solid), 1))) {
      reaction.reactants
    }

    assertResult(Seq(
      FormulaQuantity(Formula("Na{1}"), Some(Phase.Aqueous), 1),
      FormulaQuantity(Formula("Cl{-1}"), Some(Phase.Aqueous), 1))) {

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
    val reactionAsString = "NaCl (s) = Na{1} (aq) + 2 Cl{-1} (aq)"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("NaCl"), Some(Phase.Solid), 1))) {
      reaction.reactants
    }

    assertResult(Seq(
      FormulaQuantity(Formula("Na{1}"), Some(Phase.Aqueous), 1),
      FormulaQuantity(Formula("Cl{-1}"), Some(Phase.Aqueous), 2))) {

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
    val reactionAsString = "NaCl = Na{1} + Cl{-3}"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("NaCl"), None, 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Na{1}"), None, 1), FormulaQuantity(Formula("Cl{-3}"), None, 1))) {
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
    val reactionAsString = "C6H12O6 + 6 O2 = 6 CO2 + 6 H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("C6H12O6"), None, 1), FormulaQuantity(Formula("O2"), None, 6))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("CO2"), None, 6), FormulaQuantity(Formula("H2O"), None, 6))) {
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
    val reactionAsString = "C6H12O6 + 6 O2 = 4 CO2 + 6 H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("C6H12O6"), None, 1), FormulaQuantity(Formula("O2"), None, 6))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("CO2"), None, 4), FormulaQuantity(Formula("H2O"), None, 6))) {
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
    val reactionAsString = "CuCl2 = Cu{2} + 2 Cl{-1}"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("CuCl2"), None, 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Cu{2}"), None, 1), FormulaQuantity(Formula("Cl{-1}"), None, 2))) {
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
    val reactionAsString = "CuSO4 = Cu{2} + SO4{-2}"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("CuSO4"), None, 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Cu{2}"), None, 1), FormulaQuantity(Formula("SO4{-2}"), None, 1))) {
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
    val reactionAsString = "Ba(C2H3O2)2 = Ba{2} + 2 C2H3O2{-1}"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(FormulaQuantity(Formula("Ba(C2H3O2)2"), None, 1))) {
      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Ba{2}"), None, 1), FormulaQuantity(Formula("C2H3O2{-1}"), None, 2))) {
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
    val reactionAsString = "Ca5(PO4)3OH + 7 H3PO4 + 4 H2O = 5 Ca(H2PO4)2H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(
      FormulaQuantity(Formula("Ca5(PO4)3OH"), None, 1),
      FormulaQuantity(Formula("H3PO4"), None, 7),
      FormulaQuantity(Formula("H2O"), None, 4))) {

      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Ca(H2PO4)2H2O"), None, 5))) {
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
    val reactionAsString = "Ca5(PO24)3OH + 7 H3PO4 + 4 H2O = 5 Ca(H2PO4)2H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(Seq(
      FormulaQuantity(Formula("Ca5(PO24)3OH"), None, 1),
      FormulaQuantity(Formula("H3PO4"), None, 7),
      FormulaQuantity(Formula("H2O"), None, 4))) {

      reaction.reactants
    }

    assertResult(Seq(FormulaQuantity(Formula("Ca(H2PO4)2H2O"), None, 5))) {
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

  test("testMultiplyChemicalEquation") {
    val reactionAsString = "Ca5(PO4)3OH + 7 H3PO4 + 4 H2O = 5 Ca(H2PO4)2H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(true) {
      reaction.isBalanced
    }

    val reaction2 = reaction.multiplyCoefficients(5)

    assertResult(true) {
      reaction2.isBalanced
    }

    assertResult(reaction.reactants.map(fq => 5 * reaction.getReactantQuantity(fq.formula))) {
      reaction2.reactants.map(fq => reaction2.getReactantQuantity(fq.formula))
    }

    assertResult(reaction.products.map(fq => 5 * reaction.getProductQuantity(fq.formula))) {
      reaction2.products.map(fq => reaction2.getProductQuantity(fq.formula))
    }

    // Same if we take (absent) phase into account

    assertResult(reaction.reactants.map(fq => 5 * reaction.getReactantQuantity(fq.formulaPhase))) {
      reaction2.reactants.map(fq => reaction2.getReactantQuantity(fq.formulaPhase))
    }

    assertResult(reaction.products.map(fq => 5 * reaction.getProductQuantity(fq.formulaPhase))) {
      reaction2.products.map(fq => reaction2.getProductQuantity(fq.formulaPhase))
    }
  }

  test("testNormlizeChemicalEquation") {
    val reactionAsString = "3 Ca5(PO4)3OH + 21 H3PO4 + 12 H2O + 3 O2 = O2 + 15 Ca(H2PO4)2H2O + 2 O2"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(ChemicalEquation("3 Ca5(PO4)3OH + 21 H3PO4 + 12 H2O + 3 O2 = O2 + 15 Ca(H2PO4)2H2O + 2 O2")) {
      reaction.withoutDuplicateReactants
    }

    assertResult(ChemicalEquation("3 Ca5(PO4)3OH + 21 H3PO4 + 12 H2O + 3 O2 = 3 O2 + 15 Ca(H2PO4)2H2O")) {
      reaction.withoutDuplicateProducts
    }

    assertResult(ChemicalEquation("3 Ca5(PO4)3OH + 21 H3PO4 + 12 H2O = 15 Ca(H2PO4)2H2O")) {
      reaction.withoutDuplicateFormulas
    }

    assertResult(ChemicalEquation("1 Ca5(PO4)3OH + 7 H3PO4 + 4 H2O = 5 Ca(H2PO4)2H2O")) {
      reaction.withoutDuplicateFormulas.normalizeCoefficients
    }

    assertResult(ChemicalEquation("1 Ca5(PO4)3OH + 7 H3PO4 + 4 H2O = 5 Ca(H2PO4)2H2O")) {
      reaction.normalize
    }

    assertResult(true) {
      reaction.normalize.isBalanced
    }
  }

  test("testSwapChemicalEquation") {
    val reactionAsString = "1 Ca5(PO4)3OH + 7 H3PO4 + 4 H2O = 5 Ca(H2PO4)2H2O"
    val reaction = ChemicalEquation(reactionAsString)

    assertResult(true) {
      reaction.isBalanced
    }

    assertResult(ChemicalEquation("5 Ca(H2PO4)2H2O = 1 Ca5(PO4)3OH + 7 H3PO4 + 4 H2O")) {
      reaction.swapReactantsAndProducts
    }

    assertResult(reaction) {
      reaction.swapReactantsAndProducts.swapReactantsAndProducts
    }
  }

  test("testAddChemicalEquations") {
    assertResult(ChemicalEquation("C2H4 (g) + 2 H2 (g) + O2 (g) = C2H5OH (l) + H2O (l)")) {
      ChemicalEquation("C2H4 (g) + H2O (l) = C2H5OH (l)").add(ChemicalEquation("2 H2 (g) + O2 (g) = 2 H2O (l)"))
    }

    assertResult(ChemicalEquation("2 C2H4 (g) + 2 H2 (g) + O2 (g) = 2 C2H5OH (l)")) {
      ChemicalEquation("C2H4 (g) + H2O (l) = C2H5OH (l)").multiplyCoefficients(2)
        .add(ChemicalEquation("2 H2 (g) + O2 (g) = 2 H2O (l)"))
    }

    assertResult(ChemicalEquation("2 C2H4 + 2 H2 + O2 = 2 C2H5OH")) {
      ChemicalEquation("C2H4 (g) + H2O (l) = C2H5OH (l)").multiplyCoefficients(2)
        .addIgnoringPhase(ChemicalEquation("2 H2 (g) + O2 (g) = 2 H2O (l)"))
    }
  }

  test("testCombineEquations") {
    val reactions = List(
      ChemicalEquation("C6H12O6 (s) + 6 O2 (g) = 6 CO2 (g) + 6 H2O (l)"),
      ChemicalEquation("C2H4 (g) + H2O (l) = C2H5OH (l)"),
      ChemicalEquation("2 C (s) + 2 H2 (g) = C2H4 (g)"),
      ChemicalEquation("2 H2 (g) + O2 (g) = 2 H2O (l)"),
      ChemicalEquation("C (s) + O2 (g) = CO2 (g)"),
    )

    assertResult(true) {
      reactions.forall(_.isBalanced)
    }

    assertResult(ChemicalEquation("C6H12O6 (s) = 2 CO2 (g) + 2 C2H5OH (l)").ensuring(_.isBalanced)) {
      reactions(0)
        .add(reactions(1).multiplyCoefficients(2))
        .add(reactions(2).multiplyCoefficients(2))
        .add(reactions(3).swapReactantsAndProducts.multiplyCoefficients(2))
        .add(reactions(4).swapReactantsAndProducts.multiplyCoefficients(4))
    }
  }
}
