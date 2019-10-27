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
import eu.cdevreeze.introchemistry.thermochemistry.ThermochemicalEquation
import eu.cdevreeze.introchemistry.thermochemistry.ThermochemistrySupport

/**
 * Simple chemistry query API, combining the different APIs into one "entry point".
 *
 * @author Chris de Vreeze
 */
final class SimpleQueryApi(val periodicTable: PeriodicTable)
  extends SimpleStoichiometryQueryApi with SimpleOrbitalsQueryApi with SimpleThermochemistryQueryApi

object SimpleQueryApi {

  def apply(periodicTable: PeriodicTable): SimpleQueryApi = new SimpleQueryApi(periodicTable)

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
   * Adding a Phase to a formula.
   */
  implicit class AddPhase(formula: Formula) {

    def withOptPhase(phaseOption: Option[Phase]): ChemicalEquation.FormulaPhase = ChemicalEquation.FormulaPhase(formula, phaseOption)

    def withoutPhase: ChemicalEquation.FormulaPhase = withOptPhase(None)

    def asGas: ChemicalEquation.FormulaPhase = withOptPhase(Some(Phase.Gas))

    def asLiquid: ChemicalEquation.FormulaPhase = withOptPhase(Some(Phase.Liquid))

    def asSolid: ChemicalEquation.FormulaPhase = withOptPhase(Some(Phase.Solid))

    def aqueous: ChemicalEquation.FormulaPhase = withOptPhase(Some(Phase.Aqueous))
  }

  /**
   * Postfix operator to turn a string into a ChemicalEquation.
   */
  implicit class ToChemicalEquation(chemicalEquationString: String) {

    def ce: ChemicalEquation = ChemicalEquation(chemicalEquationString)
  }

  /**
   * Postfix operator to turn a ChemicalEquation and a delta of the enthalph in kJ into a ThermochemicalEquation.
   */
  implicit class ToThermochemicalEquation(chemicalEquation: ChemicalEquation) {

    def dH(deltaEnthalpyInKiloJoule: BigDecimal): ThermochemicalEquation = ThermochemicalEquation(chemicalEquation, deltaEnthalpyInKiloJoule)
  }

  val LiterInCubicMeter: BigDecimal = ThermochemistrySupport.LiterInCubicMeter

  val AtmosphereInPascal: BigDecimal = ThermochemistrySupport.AtmosphereInPascal

  val LiterAtmInJoule: BigDecimal = ThermochemistrySupport.LiterAtmInJoule

  /**
   * Small calorie in Joule. Note that a cal (small calorie) is the energy needed to increase the temperature of 1 g of water
   * by 1 Kelvin at a pressure of 1 atm. As an aside, note that a large calorie (Cal or kcal), or food calorie, is the same for 1 kg instead of g.
   */
  val SmallCalorieInJoule: BigDecimal = ThermochemistrySupport.SmallCalorieInJoule

  val LargeCalorieInJoule: BigDecimal = ThermochemistrySupport.LargeCalorieInJoule

  /**
   * The ideal gas constant, in J / (K * mol).
   */
  val IdealGasConstantInJoulePerKelvinMole: BigDecimal = ThermochemistrySupport.IdealGasConstantInJoulePerKelvinMole

}
