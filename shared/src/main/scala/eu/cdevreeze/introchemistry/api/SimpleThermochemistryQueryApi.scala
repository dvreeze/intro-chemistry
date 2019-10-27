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
import eu.cdevreeze.introchemistry.thermochemistry.ThermochemistrySupport

/**
 * Simple thermochemistry query API, to be mixed in into SimpleQueryApi.
 *
 * @author Chris de Vreeze
 */
trait SimpleThermochemistryQueryApi {

  def periodicTable: PeriodicTable

  // Energy and heat queries

  /**
   * Returns the "PV work" in Joule, from a pressure value in Pascal and a delta of the volume in cubic meter.
   * The resulting work is the pressure times -1, multiplied by the delta of the volume.
   *
   * PV work applies to expansion or compressions of gases. Note that for compression the delta of the volume is negative
   * so the work is positive, and that for expansion the delta of the volume is positive so the work is negative.
   */
  final def pvWork(pressureInPascal: BigDecimal, deltaVolumeInCubicMeter: BigDecimal): BigDecimal = {
    ThermochemistrySupport(periodicTable).pvWork(pressureInPascal, deltaVolumeInCubicMeter)
  }

  /**
   * Returns the volume of the gas in cubic meter, given the moles of gas, its temperature in Kelvin, and its pressure in Pascal.
   * The gas is assumed to be an ideal gas. That is, the ideal gas law (or general gas equation) is assumed to hold.
   * It says (assuming SI units): pressure * volume = moles * IdealGasConstantInJoulePerKelvinMole * temperature.
   * Put differently: pressure * delta-volume = delta-moles * IdealGasConstantInJoulePerKelvinMole * temperature.
   *
   * From the first of these equations it follows what result this function returns, by dividing LHS and RHS by the pressure.
   * Given the results of this function as 2 volumes (in cubic meter), and the pressure, we can compute the PV work with function pvWork.
   *
   * Note that this function and its implementation make sense from the perspective of the units used.
   * Indeed, mol * (Joule / (Kelvin * mol)) * Kelvin / Pascal = mol * N * m / (Kelvin * mol) * Kelvin / (N / (m * m)).
   * This is equal to: m * m * m, which is cubic meter.
   */
  final def volumeOfIdealGasInCubicMeter(molesOfGas: BigDecimal, temperatureInKelvin: BigDecimal, pressureInPascal: BigDecimal): BigDecimal = {
    ThermochemistrySupport(periodicTable).volumeOfIdealGasInCubicMeter(molesOfGas, temperatureInKelvin, pressureInPascal)
  }

  /**
   * Returns the delta of the energy in Joule, as the sum of the heat (in Joule) and the work (in Joule). Note that at
   * constant pressure the heat is the change of enthalpy.
   *
   * In the case of compression or expansion of gases, the work is typically PV work (see method pvWork).
   *
   * Note that work done on the system (the reaction, mostly) is positive and work done by the system on its surroundings
   * is negative. Similarly, heat is positive for endothermic reactions (absorbing heat) and negative for exothermic reactions
   * (giving off heat).
   */
  final def deltaEnergyFromHeatAndWork(heatInJoule: BigDecimal, workInJoule: BigDecimal): BigDecimal = {
    ThermochemistrySupport(periodicTable).deltaEnergyFromHeatAndWork(heatInJoule, workInJoule)
  }

  /**
   * Like method deltaEnergyFromHeatAndWork, but taking and returning kilo-Joules instead of Joules.
   */
  final def deltaEnergyFromHeatAndWorkInKiloJoule(heatInKiloJoule: BigDecimal, workInKiloJoule: BigDecimal): BigDecimal = {
    ThermochemistrySupport(periodicTable).deltaEnergyFromHeatAndWorkInKiloJoule(heatInKiloJoule, workInKiloJoule)
  }

  // Temperature conversions

  final def convertDegreesCelsiusToKelvin(temperatureInDegreesCelsius: BigDecimal): BigDecimal = {
    ThermochemistrySupport(periodicTable).convertDegreesCelsiusToKelvin(temperatureInDegreesCelsius)
  }

  final def convertKelvinToDegreesCelsius(temperatureInKelvin: BigDecimal): BigDecimal = {
    ThermochemistrySupport(periodicTable).convertKelvinToDegreesCelsius(temperatureInKelvin)
  }
}
