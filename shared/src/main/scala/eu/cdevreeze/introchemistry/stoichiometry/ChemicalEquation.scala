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
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation.FormulaPhase

/**
 * A chemical equation in stoichiometry. The reactants and products are formulas.
 *
 * See the documentation of type ChemicalEquation for more information. For the terminology, see
 * https://chem.libretexts.org/Courses/College_of_Marin/Marin%3A_CHEM_114_-_Introductory_Chemistry_(Daubenmire).
 *
 * The string format of the chemical equation is to a large extent compatible with the format used in https://www.webqc.org/balance.php.
 *
 * @author Chris de Vreeze
 */
final case class ChemicalEquation(reactants: Seq[ChemicalEquation.FormulaQuantity], products: Seq[ChemicalEquation.FormulaQuantity]) {
  require(reactants.nonEmpty, s"At least one reactant must be given")
  require(products.nonEmpty, s"At least one product must be given")

  import ChemicalEquation.FormulaQuantity

  def reactantsAndProducts: Seq[FormulaQuantity] = reactants.appendedAll(products)

  def usedElements: Set[ElementSymbol] = {
    reactantsAndProducts.map(_.formula).flatMap(_.atomCounts.keySet).toSet
  }

  def isBalanced: Boolean = {
    usedElements.forall(elemSymbol => isBalancedForElement(elemSymbol)) && isBalancedWithRespectToCharges
  }

  def isBalancedForElement(elementSymbol: ElementSymbol): Boolean = {
    val leftHandCount: Int = reactants.map(fq => fq.quantity * fq.formula.atomCount(elementSymbol)).sum
    val rightHandCount: Int = products.map(fq => fq.quantity * fq.formula.atomCount(elementSymbol)).sum
    leftHandCount == rightHandCount
  }

  def isBalancedWithRespectToCharges: Boolean = {
    val leftHandCount: Int = reactants.map(fq => fq.quantity * fq.formula.charge).sum
    val rightHandCount: Int = products.map(fq => fq.quantity * fq.formula.charge).sum
    leftHandCount == rightHandCount
  }

  def withoutPhases: ChemicalEquation = {
    new ChemicalEquation(reactants.map(_.withoutPhase), products.map(_.withoutPhase))
  }

  /**
   * Returns the quantity of the reactant(s) with the given formula, and returns 0 if not found.
   * The given formula must match the formula in the chemical equation to the letter, or else it is not found.
   * The optional phases of the formulas are ignored.
   */
  def getReactantQuantity(formula: Formula): Int = {
    reactants.filter(_.formula == formula).map(_.quantity).sum
  }

  /**
   * Returns the quantity of the product(s) with the given formula, and returns 0 if not found.
   * The given formula must match the formula in the chemical equation to the letter, or else it is not found.
   * The optional phases of the formulas are ignored.
   */
  def getProductQuantity(formula: Formula): Int = {
    products.filter(_.formula == formula).map(_.quantity).sum
  }

  /**
   * Returns the quantity of the reactant(s) with the given formula phase, and returns 0 if not found.
   * The given formula phase must match the formula phase in the chemical equation to the letter, or else it is not found.
   */
  def getReactantQuantity(formulaPhase: FormulaPhase): Int = {
    reactants.filter(_.formulaPhase == formulaPhase).map(_.quantity).sum
  }

  /**
   * Returns the quantity of the product(s) with the given formula phase, and returns 0 if not found.
   * The given formula phase must match the formula phase in the chemical equation to the letter, or else it is not found.
   */
  def getProductQuantity(formulaPhase: FormulaPhase): Int = {
    products.filter(_.formulaPhase == formulaPhase).map(_.quantity).sum
  }

  def multiplyCoefficients(factor: Int): ChemicalEquation = {
    require(factor > 0, s"Expected factor > 0")

    ChemicalEquation(
      reactants.map(_.mapQuantity(_ * factor)),
      products.map(_.mapQuantity(_ * factor)))
  }

  def normalize: ChemicalEquation = {
    this.withoutDuplicateFormulas.normalizeCoefficients
  }

  def normalizeCoefficients: ChemicalEquation = {
    val gcdOpt = gcdOption(reactantsAndProducts.map(_.quantity))

    gcdOpt.map(gcd => ChemicalEquation(reactants.map(_.mapQuantity(_ / gcd)), products.map(_.mapQuantity(_ / gcd)))).getOrElse(this)
  }

  def withoutDuplicateFormulas: ChemicalEquation = {
    val startCe = this.withoutDuplicateReactants.withoutDuplicateProducts

    val duplicateFormulaPhases: Set[FormulaPhase] =
      startCe.reactants.map(_.formulaPhase).toSet.intersect(startCe.products.map(_.formulaPhase).toSet)

    val newReactants: Seq[FormulaQuantity] = startCe.reactants.flatMap { fq =>
      if (duplicateFormulaPhases.contains(fq.formulaPhase)) {
        val otherQty = startCe.products.find(_.formulaPhase == fq.formulaPhase).map(_.quantity).getOrElse(0)

        if (fq.quantity <= otherQty) None else Some(fq.mapQuantity(_ - otherQty))
      } else {
        Some(fq)
      }
    }

    val newProducts: Seq[FormulaQuantity] = startCe.products.flatMap { fq =>
      if (duplicateFormulaPhases.contains(fq.formulaPhase)) {
        val otherQty = startCe.reactants.find(_.formulaPhase == fq.formulaPhase).map(_.quantity).getOrElse(0)

        if (fq.quantity <= otherQty) None else Some(fq.mapQuantity(_ - otherQty))
      } else {
        Some(fq)
      }
    }

    if (newReactants.isEmpty || newProducts.isEmpty) {
      startCe
    } else {
      ChemicalEquation(newReactants, newProducts)
    }
  }

  def withoutDuplicateReactants: ChemicalEquation = {
    val formulaCounts: Map[FormulaPhase, Int] = reactants.groupBy(_.formulaPhase).view.mapValues(_.map(_.quantity).sum).toMap
    val newReactants = reactants.distinctBy(_.formulaPhase).map(fq => fq.withQuantity(formulaCounts(fq.formulaPhase)))

    ChemicalEquation(newReactants, products)
  }

  def withoutDuplicateProducts: ChemicalEquation = {
    val formulaCounts: Map[FormulaPhase, Int] = products.groupBy(_.formulaPhase).view.mapValues(_.map(_.quantity).sum).toMap
    val newProducts = products.distinctBy(_.formulaPhase).map(fq => fq.withQuantity(formulaCounts(fq.formulaPhase)))

    ChemicalEquation(reactants, newProducts)
  }

  /**
   * Returns the same equation, but removing reactants that are also products. Those duplicates must match exactly
   * (including the phase, if any) or else they are not recognized as duplicates.
   */
  def withoutDuplicates: ChemicalEquation = {
    val duplicates = reactants.toSet.intersect(products.toSet)

    if (duplicates.isEmpty) this else ChemicalEquation(reactants.filterNot(duplicates), products.filterNot(duplicates))
  }

  def swapReactantsAndProducts: ChemicalEquation = {
    ChemicalEquation(reactants = this.products, products = this.reactants)
  }

  /**
   * Adds the given chemical equation to this equation, after removing phases, so matching on formulas without phases.
   * Adding means adding the reactants together as the reactants of the result equation, and doing the same for the products.
   * If both equations are balanced, then so is the result.
   */
  def addIgnoringPhase(otherChemicalEquation: ChemicalEquation): ChemicalEquation = {
    this.withoutPhases.add(otherChemicalEquation.withoutPhases)
  }

  /**
   * Adds the given chemical equation to this equation, taking phases into account, so matching on formulas including the optional phases.
   * Adding means adding the reactants together as the reactants of the result equation, and doing the same for the products.
   * Duplicate formulas (with phases) are removed in the result, both within and across LHS and RHS of the equation, without
   * changing the equation in any meaningful way.
   *
   * If both equations are balanced, then so is the result.
   */
  def add(otherChemicalEquation: ChemicalEquation): ChemicalEquation = {
    ChemicalEquation(
      plus(reactants, otherChemicalEquation.reactants),
      plus(products, otherChemicalEquation.products)).withoutDuplicateFormulas
  }

  /**
   * Adds the 2nd formula quantities to the first ones, after "de-duplication" of formulas.
   * The result contains all formulas (with phases) of either collection (without duplicates), summing up all quantities of those formulas.
   */
  private def plus(
    formulaQuantities: Seq[FormulaQuantity],
    otherFormulaQuantities: Seq[FormulaQuantity]): Seq[FormulaQuantity] = {

    val formulaPhases = formulaQuantities.appendedAll(otherFormulaQuantities).map(_.formulaPhase).distinct

    formulaPhases.map { fp =>
      val qty = formulaQuantities.filter(_.formulaPhase == fp).map(_.quantity).sum +
        otherFormulaQuantities.filter(_.formulaPhase == fp).map(_.quantity).sum

      FormulaQuantity(fp.formula, fp.phaseOption, qty)
    }
  }

  private def gcdOption(xs: Seq[Int]): Option[Int] = {
    if (xs.isEmpty) {
      None
    } else {
      if (xs.sizeIs <= 2) {
        if (xs.size == 1) Some(xs.head) else Some(gcd(xs(0), xs(1)))
      } else {
        gcdOption(xs.tail).map(n => gcd(xs.head, n)) // Recursive call
      }
    }
  }

  private def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y) // Recursive call
  }
}

object ChemicalEquation {

  final case class FormulaPhase(formula: Formula, phaseOption: Option[Phase]) {

    def withoutPhase: FormulaPhase = FormulaPhase(formula, None)
  }

  final case class FormulaQuantity(formula: Formula, phaseOption: Option[Phase], quantity: Int) {
    require(quantity > 0, s"Quantity must be > 0")

    def withQuantity(newQuantity: Int): FormulaQuantity = this.copy(quantity = newQuantity)

    def mapQuantity(f: Int => Int): FormulaQuantity = withQuantity(f(quantity))

    def formulaPhase: FormulaPhase = FormulaPhase(formula, phaseOption)

    def withoutPhase: FormulaQuantity = FormulaQuantity(formula, None, quantity)
  }

  def apply(s: String): ChemicalEquation = parse(s)

  def parse(s: String): ChemicalEquation = {
    Parser.parseChemicalEquation(s)
  }

  object Parser {

    import fastparse._, SingleLineWhitespace._

    def parseChemicalEquation(s: String): ChemicalEquation = {
      val parseResult = fastparse.parse(s, chemicalEquation(_))

      parseResult match {
        case Parsed.Success(result, _) => result
        case Parsed.Failure(_, _, _) => sys.error(s"Could not parse chemical equation from '$s'")
      }
    }

    def chemicalEquation[_: P]: P[ChemicalEquation] =
      P(reactant.rep(min = 1, sep = "+"./) ~ "=" ~ product.rep(min = 1, sep = "+"./))
        .map { case (reacts, prods) => ChemicalEquation(reacts, prods) }

    def reactant[_: P]: P[FormulaQuantity] = P(formulaQuantity)

    def product[_: P]: P[FormulaQuantity] = P(formulaQuantity)

    def formulaQuantity[_: P]: P[FormulaQuantity] =
      P(count.? ~ Formula.Parser.formula ~ phase.?)
        .map { case (cntOpt, f, phaseOpt) => FormulaQuantity(f, phaseOpt, cntOpt.getOrElse(1)) }

    def phase[_: P]: P[Phase] = P("(" ~ ("s" | "l" | "g" | "aq").! ~ ")").map(s => Phase.parse(s))

    def count[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt).filter(_ > 0)
  }

  // Builder

  final case class Builder(reactants: Seq[FormulaQuantity], products: Seq[FormulaQuantity]) {

    def build: ChemicalEquation = ChemicalEquation(reactants, products)

    def plusReactant(quantity: Int, formula: Formula, phase: Phase): Builder = {
      Builder(reactants.appended(FormulaQuantity(formula, Some(phase), quantity)), products)
    }

    def plusReactant(quantity: Int, formula: Formula): Builder = {
      Builder(reactants.appended(FormulaQuantity(formula, None, quantity)), products)
    }

    def plusReactant(quantity: Int, formulaString: String, phaseString: String): Builder = {
      plusReactant(quantity, Formula(formulaString), Phase(phaseString))
    }

    def plusReactant(quantity: Int, formulaString: String): Builder = {
      plusReactant(quantity, Formula(formulaString))
    }

    def plusProduct(quantity: Int, formula: Formula, phase: Phase): Builder = {
      Builder(reactants, products.appended(FormulaQuantity(formula, Some(phase), quantity)))
    }

    def plusProduct(quantity: Int, formula: Formula): Builder = {
      Builder(reactants, products.appended(FormulaQuantity(formula, None, quantity)))
    }

    def plusProduct(quantity: Int, formulaString: String, phaseString: String): Builder = {
      plusProduct(quantity, Formula(formulaString), Phase(phaseString))
    }

    def plusProduct(quantity: Int, formulaString: String): Builder = {
      plusProduct(quantity, Formula(formulaString))
    }
  }

  def builder: Builder = Builder(Seq.empty, Seq.empty)

}
