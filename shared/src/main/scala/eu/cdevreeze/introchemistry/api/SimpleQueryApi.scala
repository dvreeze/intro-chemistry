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

package eu.cdevreeze.introchemistry.api

import eu.cdevreeze.introchemistry.periodictable.Element
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.periodictable.OxidationStates
import eu.cdevreeze.introchemistry.periodictable.PeriodicTable
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation
import eu.cdevreeze.introchemistry.stoichiometry.Formula
import eu.cdevreeze.introchemistry.stoichiometry.Phase
import eu.cdevreeze.introchemistry.stoichiometry.StoichiometrySupport

/**
 * Simple chemistry query API, combining the different APIs into one "entry point".
 *
 * @author Chris de Vreeze
 */
final class SimpleQueryApi(val periodicTable: PeriodicTable) {

  // PeriodicTable queries

  def getElement(elementSymbol: ElementSymbol): Element = {
    periodicTable.getElement(elementSymbol)
  }

  def getElementByAtomicNumber(atomicNumber: Int): Element = {
    periodicTable.getElementByAtomicNumber(atomicNumber)
  }

  def getElementByName(name: String): Element = {
    periodicTable.getElementByName(name)
  }

  // OxidationStates queries

  /**
   * Tries to find the probable or mandatory oxidation state of an element in a compound (not in an element itself because
   * then the oxidation state is 0).
   */
  def findProbableOxidationNumber(elementSymbol: ElementSymbol): Option[Int] = {
    OxidationStates.findProbableOxidationNumber(elementSymbol)
  }

  // StoichiometrySupport

  def avogadrosNumber: BigDecimal = StoichiometrySupport.AvogadrosNumber

  /**
   * The mass of a formula in atomic mass units (or Daltons).
   */
  def massInAmu(formula: Formula): BigDecimal = {
    StoichiometrySupport(periodicTable).massInAmu(formula)
  }

  /**
   * Returns `massInAmu(formula)`, interpreted as the mass in grams per mole. That is the beauty of the quantity
   * of a Mole (Avogadro's number as quantity): grams per mole of the formula equals the atomic mass (amu) of the formula.
   */
  def massInGramPerMole(formula: Formula): BigDecimal = {
    StoichiometrySupport(periodicTable).massInGramPerMole(formula)
  }

  /**
   * The mass of an atom in atomic mass units (or Daltons).
   */
  def massOfAtomInAmu(elementSymbol: ElementSymbol): BigDecimal = {
    StoichiometrySupport(periodicTable).massOfAtomInAmu(elementSymbol)
  }

  /**
   * Returns `massOfAtomInAmu(elementSymbol)`, interpreted as the mass in grams per mole. That is the beauty of the quantity
   * of a Mole (Avogadro's number as quantity): grams per mole of the atom equals the atomic mass (amu) of the atom.
   */
  def massOfAtomInGramPerMole(elementSymbol: ElementSymbol): BigDecimal = {
    StoichiometrySupport(periodicTable).massOfAtomInGramPerMole(elementSymbol)
  }

  // Other queries

  /**
   * Returns the 7 diatomic elements as element symbols.
   */
  def diatomicElements: Set[ElementSymbol] = ElementSymbol.diatomicElements

  // Balancing chemical equations

  /**
   * Tries to balance the given (probably unbalanced) chemical equation, returning the optional balanced result if
   * successful.
   */
  def tryToBalanceChemicalEquation(equation: ChemicalEquation): Option[ChemicalEquation] = {
    StoichiometrySupport(periodicTable).tryToBalanceChemicalEquation(equation)
  }

  // Conversions from strings to formulas or chemical equations

  /**
   * Postfix operator to turn a string into a Formula.
   */
  implicit class ToFormula(formulaString: String) {

    def f: Formula = Formula(formulaString)
  }

  /**
   * Postfix operator to turn a string into a Phase.
   */
  implicit class ToPhase(phaseString: String) {

    def p: Phase = Phase(phaseString)
  }

  /**
   * Postfix operator to turn a string into a ChemicalEquation.
   */
  implicit class ToChemicalEquation(chemicalEquationString: String) {

    def ce: ChemicalEquation = ChemicalEquation(chemicalEquationString)
  }

}
