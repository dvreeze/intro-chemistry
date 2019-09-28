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
package eu.cdevreeze.introchemistry.periodictable

/**
 * Oxidation state rules. Note that hydrogen is +1 when binding to non-metals and -1 when binding to most metals.
 * Also note that the oxidation number of the atoms in an element is always 0. These 2 rules are not encoded in the
 * method findProbableOxidationNumber below.
 *
 * @author Chris de Vreeze
 */
object OxidationStates {

  import ElementSymbol._

  /**
   * Tries to find the probable or mandatory oxidation state of an element in a compound (not in an element itself because
   * then the oxidation state is 0). Note that especially for transition metals, multiple oxidation numbers are possible, in
   * which case None is returned by this method.
   */
  def findProbableOxidationNumber(elementSymbol: ElementSymbol): Option[Int] = {
    elementSymbol match {
      case F | Cl | Br | I => Some(getOxidationNumberOfFluorine)
      case O | S | Se => Some(getOxidationNumberOfOxygen)
      case Al => Some(getOxidationNumberOfAluminium)
      case elemSym if ElementSymbol.halogens.contains(elemSym) => Some(getOxidationNumberOfHalogens)
      case Ag => Some(getOxidationNumberOfSilver)
      case Zn => Some(getOxidationNumberOfZinc)
      case Sc => Some(getOxidationNumberOfScandium)
      case _ =>
        elementSymbol.chemicalGroup match {
          case ChemicalGroupBlock.AlkaliMetal => Some(getOxidationNumberOfAlkaliMetals)
          case ChemicalGroupBlock.AlkalineEarthMetal => Some(getOxidationNumberOfAlkalineEarthMetals)
          case ChemicalGroupBlock.NobleGas => Some(0) // After all, noble gases do not react
          case _ => None
        }
    }
  }

  def getOxidationNumberOfFluorine: Int = -1

  def getOxidationNumberOfAlkaliMetals: Int = +1

  def getOxidationNumberOfAlkalineEarthMetals: Int = +2

  def getOxidationNumberOfAluminium: Int = +3

  def getOxidationNumberOfOxygen: Int = -2

  def getOxidationNumberOfHalogens: Int = -1

  /**
   * Returns the probable oxidation number of silver, which is +1, although it is a transition metal (which normally
   * has more than one possible oxidation state).
   */
  def getOxidationNumberOfSilver: Int = +1

  /**
   * Returns the probable oxidation number of zinc, which is +2, although it is a transition metal (which normally
   * has more than one possible oxidation state).
   */
  def getOxidationNumberOfZinc: Int = +2

  /**
   * Returns the probable oxidation number of scandium, which is +3, although it is a transition metal (which normally
   * has more than one possible oxidation state).
   */
  def getOxidationNumberOfScandium: Int = +3

  // Other oxidation state rules

  def getOxidationNumberOfAtomInElement: Int = 0

  def getProbableOxidationNumberOfHydrogenInMetals: Int = -1

  def getOxidationNumberOfHydrogenInNonMetals: Int = +1
}
