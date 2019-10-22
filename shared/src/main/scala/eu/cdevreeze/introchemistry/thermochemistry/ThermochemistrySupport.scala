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

import eu.cdevreeze.introchemistry.periodictable.PeriodicTable

/**
 * Thermochemistry calculations support, given a periodic table. Highly inspired by the course material of the Chemistry course
 * from Kentucky University.
 *
 * Notice the beauty of units of measurements. There are only 7 base SI units, and the rest is derived. The base units are
 * second (s), meter (m), kilogram (kg), ampere (A), kelvin (K), mole (mol) and candela (cd). Some derived units of interest here
 * are newton (N) for force, pascal (Pa) for pressure and joule (J) for energy (work or heat).
 *
 * Newton is kg * m / (s * s). This makes sense, because more force is needed to push more weight, and more force is needed
 * to increase the acceleration.
 *
 * Joule is Nm (that is, N * m), so it is equivalent to kg * m * m / (s * s). It makes sense that energy depends both on
 * force and on distance.
 *
 * Pascal is N  / (m * m). This also makes sense, because pressure is force per unit of area (m * m).
 *
 * We could have benefited from a Scala library that supports calculations with units, but that's still a possibility.
 *
 * Some core concepts pertaining to thermochemistry are:
 *  - thermodynamics: study of interconversion of heat and other kinds of energy
 *  - thermochemistry (subtopic of thermodynamics): study of heat change in chemical reactions
 *  - energy: the capacity to do work
 *  - work: directed energy change resulting from a process (or: force times distance in the direction of the force)
 *  - kinetic energy (produced by moving object) versus potential energy (energy available by virtue of object's position)
 *  - other classification of energy (where is it stored?): chemical energy, thermal energy, radiant energy, etc.
 *  - heat: transfer of thermal energy between 2 bodies at different temperatures
 *  - heat exchange occurs between system (part of universe of interest, e.g. chemical reaction) and its surroundings (the rest of the universe)
 *  - types of systems: open (e.g. gas can escape), closed (e.g. heat can enter or leave) and isolated
 *  - endothermic and exothermic process: absorbs heat and gives off heat, respectively
 *
 * Law of conservation of energy (1st law of thermodynamics): energy can be converted from one form to another, but energy cannot
 * be created or destroyed. (Interesting: how was this determined, and by whom?)
 *
 * Short introduction to thermodynamics:
 *  - state of the system consists of composition, energy, temperature, pressure etc.
 *  - state functions only depend on the state, regardless of how it was achieved; e.g. volume, total internal energy, temperature, pressure
 *  - for state functions, the magnitude of change only depends on final and initial values ("path-independent")
 *  - state functions are typically written in capitals
 *  - we cannot determine the total amount of energy in the universe, but we can determine energy changes (of systems)
 *  - so, the energy change of the system is the energy change of its surroundings negated
 *  - energy can enter of leave systems either by heat or work, so the energy change of the system is the sum of heat (q) and work (w)
 *  - heat and work are not state functions
 *  - heat is positive if endothermic, and work is positive if done by surroundings on the system
 *
 * Work is force times distance, or N times m, which is Nm, or Joule (J). When working with gases, work can be defined as
 * the negated pressure times the change in volume. That is, w equals -P * delta(V). This work is known as "PV-work".
 *
 * Does that make sense in terms of units? Yes, it does. Pascal is N / (m * m). Volume is m * m * m. So work has unit
 * Nm, or Joule, by multiplying these 2 units.
 *
 * Typically work is given in L * atm, that is, liters times atmosphere.
 *
 * @author Chris de Vreeze
 */
final class ThermochemistrySupport(val periodicTable: PeriodicTable) {

  /**
   * Returns the PV work in Joule, from a pressure value in Pascal and a delta of the volume in cubic meter.
   */
  def pvWork(pressureInPascal: BigDecimal, deltaVolumeInCubicMeter: BigDecimal): BigDecimal = {
    -pressureInPascal * deltaVolumeInCubicMeter
  }
}

object ThermochemistrySupport {

  def apply(periodicTable: PeriodicTable): ThermochemistrySupport = new ThermochemistrySupport(periodicTable)

  val LiterInCubicMeter: BigDecimal = BigDecimal(1e-3) // after all, a liter is 0.1 meter raised to the 3rd power

  val AtmosphereInPascal: BigDecimal = BigDecimal(1.01325e5)

  val LiterAtmInJoule: BigDecimal = LiterInCubicMeter * AtmosphereInPascal // 101.3
}
