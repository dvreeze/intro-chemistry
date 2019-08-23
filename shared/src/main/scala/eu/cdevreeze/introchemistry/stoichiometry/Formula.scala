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
 * A chemical formula in stoichiometry. The reactants and products are formula units.
 *
 * See the documentation of type FormulaUnit for more information.
 *
 * @author Chris de Vreeze
 */
final case class Formula(reactants: Seq[Formula.FormulaUnitQuantity], products: Seq[Formula.FormulaUnitQuantity]) {
  require(reactants.nonEmpty, s"At least one reactant must be given")
  require(products.nonEmpty, s"At least one product must be given")

  def reactantsAndProducts: Seq[Formula.FormulaUnitQuantity] = reactants.appendedAll(products)

  def usedElements: Set[ElementSymbol] = {
    reactantsAndProducts.map(_.formulaUnit).flatMap(_.atomCounts.keySet).toSet
  }

  def isBalanced: Boolean = {
    usedElements.forall(elemSymbol => isBalancedForElement(elemSymbol)) &&
      isBalancedWithRespectToCharges(reactants) &&
      isBalancedWithRespectToCharges(products)
  }

  def isBalancedForElement(elementSymbol: ElementSymbol): Boolean = {
    val leftHandCount: Int = reactants.map(fuq => fuq.quantity * fuq.formulaUnit.atomCount(elementSymbol)).sum
    val rightHandCount: Int = products.map(fuq => fuq.quantity * fuq.formulaUnit.atomCount(elementSymbol)).sum
    leftHandCount == rightHandCount
  }

  def isBalancedWithRespectToCharges(productsOrReactants: Seq[Formula.FormulaUnitQuantity]): Boolean = {
    productsOrReactants.map(fuq => fuq.quantity * fuq.formulaUnit.charge).sum == 0
  }

  /**
   * Returns the string representation from which the same formula can be parsed.
   */
  def show: String = {
    s"${reactants.map(_.show).mkString(" + ")} --> ${products.map(_.show).mkString(" + ")}"
  }
}

object Formula {

  final case class FormulaUnitQuantity(formulaUnit: FormulaUnit, quantity: Int) {
    require(quantity > 0, s"Quantity must be > 0")

    def show: String = {
      s"$quantity ${formulaUnit.show}"
    }
  }

  def apply(s: String): Formula = parse(s)

  def parse(s: String): Formula = {
    Parser.parseFormula(s)
  }

  object Parser {

    import fastparse._, SingleLineWhitespace._

    def parseFormula(s: String): Formula = {
      val parseResult = fastparse.parse(s, formula(_))

      parseResult match {
        case Parsed.Success(result, _) => result
        case Parsed.Failure(_, _, _) => sys.error(s"Could not parse formula from '$s'")
      }
    }

    def formula[_: P]: P[Formula] =
      P(reactant.rep(min = 1, sep = "+" ./) ~ "-->" ~ product.rep(min = 1, sep = "+" ./))
        .map { case (reacts, prods) => Formula(reacts, prods) }

    def reactant[_: P]: P[FormulaUnitQuantity] = P(formulaUnitQuantity)

    def product[_: P]: P[FormulaUnitQuantity] = P(formulaUnitQuantity)

    def formulaUnitQuantity[_: P]: P[FormulaUnitQuantity] =
      P(count ~/ FormulaUnit.Parser.formulaUnit)
        .map { case (cnt, fu) => FormulaUnitQuantity(fu, cnt) }

    def count[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt).filter(_ > 0)
  }

}
