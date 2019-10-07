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

  def reactantsAndProducts: Seq[ChemicalEquation.FormulaQuantity] = reactants.appendedAll(products)

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

  def multiplyCoefficients(factor: Int): ChemicalEquation = {
    require(factor > 0, s"Expected factor > 0")

    ChemicalEquation(
      reactants.map(fq => fq.copy(quantity = factor * fq.quantity)),
      products.map(fq => fq.copy(quantity = factor * fq.quantity)))
  }

  /**
   * Returns the same equation, but removing reactants that are also products. Those duplicates must match exactly
   * (including the phase, if any) or else they are not recognized as duplicates.
   */
  def withoutDuplicates: ChemicalEquation = {
    val duplicates = reactants.toSet.intersect(products.toSet)

    if (duplicates.isEmpty) this else ChemicalEquation(reactants.filterNot(duplicates), products.filterNot(duplicates))
  }

  /**
   * Returns the string representation from which the same reaction equation can be parsed.
   */
  def show: String = {
    s"${reactants.map(_.show).mkString(" + ")} = ${products.map(_.show).mkString(" + ")}"
  }
}

object ChemicalEquation {

  final case class FormulaQuantity(formula: Formula, phaseOption: Option[Phase], quantity: Int) {
    require(quantity > 0, s"Quantity must be > 0")

    def show: String = {
      val phaseString = phaseOption.map(ph => s"($ph)").getOrElse("")

      s"$quantity ${formula.show} $phaseString".trim
    }

    def withQuantity(newQuantity: Int): FormulaQuantity = this.copy(quantity = newQuantity)
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
      P(count ~/ Formula.Parser.formula ~ phase.?)
        .map { case (cnt, f, phaseOpt) => FormulaQuantity(f, phaseOpt, cnt) }

    def phase[_: P]: P[Phase] = P("(" ~ ("s" | "l" | "g" | "aq").! ~ ")").map(s => Phase.parse(s))

    def count[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt).filter(_ > 0)
  }

  // Builder

  final case class Builder(reactants: Seq[ChemicalEquation.FormulaQuantity], products: Seq[ChemicalEquation.FormulaQuantity]) {

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
