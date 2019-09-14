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

package eu.cdevreeze.introchemistry.particlesandwaves

/**
 * Properties of waves.
 *
 * This is from the course Chemistry, from the University of Kentucky.
 *
 * And, yes, I know, there are some nice Scala libraries available to compute with units in a type-safe manner, which
 * is inherently much safer than using no units in the computations.
 *
 * @author Chris de Vreeze
 */
object WaveProperties {

  /**
   * Given the frequency in Herz and the wave length in meter, returns the speed in meter per second.
   */
  def getSpeed(frequency: BigDecimal, waveLength: BigDecimal): BigDecimal = {
    frequency * waveLength
  }

  /**
   * Given the wave length in meter and the speed in meter per second, returns the frequency in Herz.
   */
  def convertWaveLengthToFrequency(waveLength: BigDecimal, speed: BigDecimal): BigDecimal = {
    speed / waveLength
  }

  /**
   * Given the frequency in Herz and the speed in meter per second, returns the wave length in meter.
   */
  def convertFrequencyToWaveLength(frequency: BigDecimal, speed: BigDecimal): BigDecimal = {
    speed / frequency
  }

  // Electromagnetic radiation, using the speed of light as speed

  /**
   * Given the wave length in meter (and the implicit speed of light in meter per second), returns the frequency in Herz.
   */
  def convertWaveLengthToFrequency(waveLength: BigDecimal): BigDecimal = {
    convertWaveLengthToFrequency(waveLength, speedOfLightInMeterPerSecond)
  }

  /**
   * Given the frequency in Herz (and the implicit speed of light in meter per second), returns the wave length in meter.
   */
  def convertFrequencyToWaveLength(frequency: BigDecimal): BigDecimal = {
    convertFrequencyToWaveLength(frequency, speedOfLightInMeterPerSecond)
  }

  /**
   * Given the frequency of light emitted (in Herz), returns the energy of a single quanta of energy in Joule.
   * In other words, returns the energy of one photon particle in Joule, depending on the frequency of the light.
   * The result follows from the frequency by multiplying it by the Planck constant.
   */
  def getPhotonEnergy(frequency: BigDecimal): BigDecimal = {
    planckConstant * frequency
  }

  /**
   * The reverse of method getPhotonEnergy.
   */
  def convertPhotonEnergyToFrequency(photonEnergy: BigDecimal): BigDecimal = {
    photonEnergy / planckConstant
  }

  /**
   * Given a mass in gram and a speed in meter per second, computes the "de Broglie" wave length in meter.
   * It is the Planck constant divided by the product of mass (converted to kg) and speed. Note that the units are correct in this equation.
   */
  def getDeBroglieWaveLength(mass: BigDecimal, speed: BigDecimal): BigDecimal = {
    planckConstant / (mass * BigDecimal("1e-3")  * speed)
  }

  /**
   * Given the level, or shell (or principal quantum number), of an electron in a hydrogen atom, returns the energy
   * of that electron in Joule. It is the negated Rydberg constant for hydrogen divided by the square of the level.
   */
  def getEnergyOfElectronInHydrogen(level: Int): BigDecimal = {
    -rydbergConstantForHydrogen / (level * level)
  }

  val visibleLightSpectrumInMeters: (BigDecimal, BigDecimal) = (BigDecimal("380e-9"), BigDecimal("700e-9"))

  /**
   * Speed of light in a vacuum
   */
  val speedOfLightInMeterPerSecond: BigDecimal = BigDecimal(299792458L)

  /**
   * Planck constant, in Joule times seconds. Note that Joule is (kg * m * m / (s * s)).
   */
  val planckConstant: BigDecimal = BigDecimal("6.62607004e-34")

  /**
   * The Rydberg constant for hydrogen, in Joule.
   */
  val rydbergConstantForHydrogen: BigDecimal = BigDecimal("2.18e-18")
}
