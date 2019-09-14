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

package eu.cdevreeze.introchemistry.orbitals

/**
 * Set of 4 quantum numbers, namely the principal quantum number ("n"), the angular momentum quantum number ("l"),
 * the magnetic quantum number ("m-l"), and the spin quantum number ("m-s"). The first 3 quantum numbers determine
 * the orbital of the electron, and the spin quantum number determines the orientation of the spin axis. Each orbital can have
 * at most 2 electrons, and they must have opposite orientation (that is, spin). Indeed, the Pauli exclusion principle
 * states that no 2 electrons in the same atom can have the same sets of 4 quantum numbers. Hence at most 2 electrons can
 * occupy the same orbital (see below), and these 2 electrons must have different spin quantum number.
 *
 * The principal quantum number determines the (average) distance of the electron from the nucleus. Hence it determines the energy
 * of the electron (to a large extent) and the size of the orbital. All orbitals with the same principal quantum number are
 * said to be in the same shell.
 *
 * The angular momentum quantum number determines the shape of the orbital, given the principal quantum number. Orbitals with
 * the same principal and angular momentum quantum numbers are said to be in the same subshell.
 *
 * The magnetic quantum number distinguishes electrons in the same subshell on their orientation in space. It divides the
 * subshell into different orbitals.
 *
 * The spin quantum number distinguishes the 2 electrons that can occupy the same orbital, based on the orientation of the
 * spin axis.
 *
 * See for example https://www.angelo.edu/faculty/kboudrea/general/quantum_numbers/Quantum_Numbers.htm.
 *
 * @author Chris de Vreeze
 */
final case class QuantumNumberSet(
  principalQuantumNumber: Int,
  angularMomentumQuantumNumber: Int,
  magneticQuantumNumber: Int,
  spinQuantumNumber: Spin
) {

  require(
    principalQuantumNumber > 0,
    s"The principal quantum number must be at least 1, but got $principalQuantumNumber instead")

  require(
    angularMomentumQuantumNumber >= 0,
    s"The angular momentum quantum number must be at least 0, but got $angularMomentumQuantumNumber instead")
  require(
    angularMomentumQuantumNumber < principalQuantumNumber,
    s"The angular momentum quantum number must be at most ${principalQuantumNumber - 1}, but got $angularMomentumQuantumNumber instead")

  require(
    magneticQuantumNumber.abs <= angularMomentumQuantumNumber,
    s"The absolute value of the magnetic quantum number must be at most $angularMomentumQuantumNumber, but got $magneticQuantumNumber instead"
  )

  def subshell: Subshell = Subshell.fromSublevel(angularMomentumQuantumNumber)

  /**
   * Returns the orbital name, which more precisely should be called the subshell name.
   */
  def orbitalName: String = {
    s"$principalQuantumNumber${subshell.name}"
  }
}

object QuantumNumberSet {

  /**
   * Returns an optional QuantumNumberSet, which is filled if and only if the quantum numbers together form a consistent
   * (that is, allowed) quantum number set.
   */
  def opt(
    principalQuantumNumber: Int,
    angularMomentumQuantumNumber: Int,
    magneticQuantumNumber: Int,
    spinQuantumNumber: Spin): Option[QuantumNumberSet] = {

    principalQuantumNumber match {
      case n if n < 0 => None
      case n =>
        angularMomentumQuantumNumber match {
          case l if l < 0 || l >= principalQuantumNumber => None
          case l =>
            magneticQuantumNumber match {
              case ml if ml.abs > angularMomentumQuantumNumber => None
              case ml =>
                Some(QuantumNumberSet(principalQuantumNumber, angularMomentumQuantumNumber, magneticQuantumNumber, spinQuantumNumber))
            }
        }
    }
  }
}
