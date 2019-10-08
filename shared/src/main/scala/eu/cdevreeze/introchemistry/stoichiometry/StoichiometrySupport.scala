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

import scala.util.Try

import eu.cdevreeze.introchemistry.internal.GaussianElimination
import eu.cdevreeze.introchemistry.internal.Matrix
import eu.cdevreeze.introchemistry.periodictable.Element
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.periodictable.PeriodicTable

/**
 * Stoichiometry support, given a periodic table. In perticular, determining the mass of a molecule and conversions between
 * mass and moles are supported. The atom counts in a molecule can be queried directly on the Formula. Balancing
 * chemical equations is also supported.
 *
 * @author Chris de Vreeze
 */
final class StoichiometrySupport(val periodicTable: PeriodicTable) {

  /**
   * The mass of a formula in atomic mass units (or Daltons).
   */
  def massInAmu(formula: Formula): BigDecimal = {
    formula.atomCounts.map { case (elementSymbol: ElementSymbol, atomCount: Int) =>
      val atomMassOfAtom: BigDecimal = massOfAtomInAmu(elementSymbol)
      atomMassOfAtom * atomCount
    }.sum
  }

  /**
   * Returns `massInAmu(formula)`, interpreted as the mass in grams per mole. That is the beauty of the quantity
   * of a Mole (Avogadro's number as quantity): grams per mole of the formula equals the atomic mass (amu) of the formula.
   */
  def massInGramPerMole(formula: Formula): BigDecimal = {
    massInAmu(formula)
  }

  /**
   * The mass of an atom in atomic mass units (or Daltons).
   */
  def massOfAtomInAmu(elementSymbol: ElementSymbol): BigDecimal = {
    val element: Element = periodicTable.getElement(elementSymbol)
    val atomMass: BigDecimal = element.atomicMass
    atomMass
  }

  /**
   * Returns `massOfAtomInAmu(elementSymbol)`, interpreted as the mass in grams per mole. That is the beauty of the quantity
   * of a Mole (Avogadro's number as quantity): grams per mole of the atom equals the atomic mass (amu) of the atom.
   */
  def massOfAtomInGramPerMole(elementSymbol: ElementSymbol): BigDecimal = {
    massOfAtomInAmu(elementSymbol)
  }

  /**
   * Tries to balance the given (probably unbalanced) chemical equation, returning the optional balanced result if
   * successful.
   */
  def tryToBalanceChemicalEquation(equation: ChemicalEquation): Option[ChemicalEquation] = {
    // Find the elements and keep the order. Each element becomes an equation for that element.
    val elements: Seq[ElementSymbol] = equation.reactantsAndProducts.flatMap(_.formula.elementSymbols).distinct

    // The columns are quantities of reactants, quantities of products negated, and the last column is zero.
    val rowsForElements: Seq[Seq[Long]] = elements.map { element =>
      val reactantColumns = equation.reactants.map(_.formula.atomCount(element).toLong)
      val productColumns = equation.products.map(_.formula.atomCount(element).toLong).map(q => -q)

      reactantColumns.appendedAll(productColumns).appended(0L)
    }

    // We ignore the charges here. Maybe we shouldn't, because it makes an extra equation.

    val columnCount = equation.reactants.size + equation.products.size + 1
    val maxRowCount = columnCount - 1
    val rows = rowsForElements.take(maxRowCount)

    val matrix = Matrix[Long](rows)

    val resultMatrix = GaussianElimination.tryToBalanceEquations(matrix, maxParValue)

    val resultEquationOption: Option[ChemicalEquation] =
      Try {
        if (GaussianElimination.isFoundToHaveExactlyOneNonZeroIntegersOnlySolution(resultMatrix)) {
          val quantities: Seq[Int] = resultMatrix.map(_.toInt).rows.sortBy(_.takeWhile(_ == 0).size).map { row =>
            val coefficient = row.init.find(_ != 0).ensuring(_.nonEmpty).get
            require(row.last % coefficient == 0, s"${row.last} not divisable by $coefficient")
            (row.last / coefficient).ensuring(_ > 0)
          }

          val reactantsAndProducts = equation.reactantsAndProducts.zip(quantities).map { case (reactantOrProduct, quantity) =>
            reactantOrProduct.withQuantity(quantity)
          }.ensuring(_.size == equation.reactantsAndProducts.size)

          Some(ChemicalEquation(
            reactantsAndProducts.take(equation.reactants.size),
            reactantsAndProducts.drop(equation.reactants.size)))
        } else {
          None
        }
      }.toOption.flatten

    resultEquationOption.filter(_.isBalanced)
  }

  private val maxParValue: Long = 5000L
}

object StoichiometrySupport {

  /**
   * Avogadro's number, which as quantity is a mole, like the number 12 is a dozen and the number 2 is a pair.
   */
  val AvogadrosNumber: BigDecimal = BigDecimal(6.02214076E23)

  def apply(periodicTable: PeriodicTable): StoichiometrySupport = new StoichiometrySupport(periodicTable)
}
