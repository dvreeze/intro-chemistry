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

package eu.cdevreeze.introchemistry.lewis

import eu.cdevreeze.introchemistry.lewis.LewisStructure.Atom
import eu.cdevreeze.introchemistry.lewis.LewisStructure.AtomKey
import eu.cdevreeze.introchemistry.lewis.LewisStructure.Bond
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * Representation of a Lewis structure.
 *
 * @author Chris de Vreeze
 */
final class LewisStructure(
  val atoms: Seq[Atom],
  val bonds: Seq[Bond],
  val netCharge: Int
) {
  require(
    bonds.flatMap(b => List(b.from, b.to)).toSet.subsetOf(atoms.map(_.key).toSet),
    s"Corrupt Lewis structure, because not all bonds refer to atoms in the Lewis structure")
  require(atoms.size == atoms.map(_.key).distinct.size, s"Duplicate atom keys not allowed")

  /**
   * Returns the atom counts per element. It can be matched against the atom counts of a corresponding formula.
   */
  def atomCounts: Map[ElementSymbol, Int] = atoms.groupBy(_.key.element).view.mapValues(_.size).toMap

  def getAtom(key: AtomKey): Atom = atoms.find(_.key == key).getOrElse(sys.error(s"Could not find atom $key"))

  def findBonds(key: AtomKey): Seq[Bond] = bonds.filter(_.touches(key))

  def getElectronCount(key: AtomKey): Int = getAtom(key).loneElectronCount + findBonds(key).size

  /**
   * Returns a count of the own and shared electrons of the atom. Typically the result should be 8, as per the octet rule.
   */
  def getSurroundingElectronCount(key: AtomKey): Int = getAtom(key).loneElectronCount + 2 * findBonds(key).size
}

object LewisStructure {

  final case class AtomKey(element: ElementSymbol, seqNr: Int)

  final case class Atom(key: AtomKey, loneElectronCount: Int)

  /**
   * Bond, with a from and to, although the direction does not matter.
   */
  final class Bond(val from: AtomKey, val to: AtomKey) {
    require(from != to, s"A bond cannot connect an atom to itself")

    def touches(key: AtomKey): Boolean = key == from || key == to
  }

  object Bond {

    def apply(from: AtomKey, to: AtomKey): Bond = new Bond(from, to)
  }
}
