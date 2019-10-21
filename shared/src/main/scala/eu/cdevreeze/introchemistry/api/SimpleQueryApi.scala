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

import eu.cdevreeze.introchemistry.periodictable.PeriodicTable
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation
import eu.cdevreeze.introchemistry.stoichiometry.Formula
import eu.cdevreeze.introchemistry.stoichiometry.Phase

/**
 * Simple chemistry query API, combining the different APIs into one "entry point".
 *
 * @author Chris de Vreeze
 */
final class SimpleQueryApi(val periodicTable: PeriodicTable) extends SimpleStoichiometryQueryApi with SimpleOrbitalsQueryApi

object SimpleQueryApi {

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
