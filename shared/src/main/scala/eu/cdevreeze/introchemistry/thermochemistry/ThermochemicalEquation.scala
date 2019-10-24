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

package eu.cdevreeze.introchemistry.thermochemistry

import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation

/**
 * A thermochemical equation. It is a balanced chemical equation along with an enthalpy change in kJ (not in Joule!).
 * All reactants and products must have an explicitly filled in phase.
 *
 * The equation typically but not necessarily describes a chemical reaction. It could also describe a phase change, like from ice to liquid water.
 * The enthalpy is positive for an endothermic reaction. It is negative for an exothermic reaction.
 *
 * Note that the coefficients of the reactants and products are moles in a thermochemical equation, and not individual molecules.
 *
 * If the equation is reversed, only the sign of the enthalpy change must change (not the absolute number of this enthalpy change).
 *
 * If both sides of the equation are multiplied by some number (multiplying all coefficients with that number), then the enthalpy
 * change must be multiplied by that same number.
 *
 * @author Chris de Vreeze
 */
final case class ThermochemicalEquation(underlyingEquation: ChemicalEquation, deltaEnthalpyInKiloJoule: BigDecimal) {
  require(underlyingEquation.isBalanced, s"Not a balanced chemical equation: $underlyingEquation")
  require(
    underlyingEquation.reactantsAndProducts.forall(_.phaseOption.nonEmpty),
    s"Missing phase in at least one reactant/product. Equation: $underlyingEquation")

}

object ThermochemicalEquation {

  def usingKiloJoule(underlyingEquation: ChemicalEquation, deltaEnthalpyInKiloJoule: BigDecimal): ThermochemicalEquation = {
    ThermochemicalEquation(underlyingEquation, deltaEnthalpyInKiloJoule)
  }

  def usingJoule(underlyingEquation: ChemicalEquation, deltaEnthalpyInJoule: BigDecimal): ThermochemicalEquation = {
    ThermochemicalEquation(underlyingEquation, deltaEnthalpyInJoule / 1000)
  }
}
