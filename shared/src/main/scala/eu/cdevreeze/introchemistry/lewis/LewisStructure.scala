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

  require(atoms.nonEmpty, s"Expected at least one atom")
  require(atoms.size == atoms.map(_.key).distinct.size, s"Duplicate atom keys not allowed")

  /**
   * Returns the atom counts per element. It can be matched against the atom counts of a corresponding formula.
   */
  def atomCounts: Map[ElementSymbol, Int] = atoms.groupBy(_.key.element).view.mapValues(_.size).toMap

  def getAtom(key: AtomKey): Atom = atoms.find(_.key == key).getOrElse(sys.error(s"Could not find atom $key"))

  def findBonds(key: AtomKey): Seq[Bond] = bonds.filter(_.touches(key))

  def getElectronCount(key: AtomKey): Int = getAtom(key).loneElectronCount + findBonds(key).size

  def electronCount: Int = atoms.map(atom => getElectronCount(atom.key)).sum - netCharge

  /**
   * Returns a count of the own and shared electrons of the atom. Typically the result should be 8, as per the octet rule.
   */
  def getSurroundingElectronCount(key: AtomKey): Int = getAtom(key).loneElectronCount + 2 * findBonds(key).size

  def obeysOctetRule(key: AtomKey): Boolean = getSurroundingElectronCount(key) == 8

  /**
   * A Lewis structure must be a connected undirected graph. This function returns true if that is indeed the case.
   */
  def isConnectedGraph: Boolean = {
    val bondsByAtomFroms: Map[AtomKey, Seq[Bond]] = bonds.groupBy(_.from)
    val bondsByAtomTos: Map[AtomKey, Seq[Bond]] = bonds.groupBy(_.to)

    val bondsByAtomKeys: Map[AtomKey, Seq[Bond]] =
      bondsByAtomFroms.keySet.union(bondsByAtomTos.keySet).toSeq
        .map(ak => ak -> bondsByAtomFroms.getOrElse(ak, Seq.empty).appendedAll(bondsByAtomTos.getOrElse(ak, Seq.empty)).distinct)
        .toMap

    val firstAtomKey: AtomKey = atoms.head.key

    val connectedAtomsOrSelf: Set[AtomKey] = findAllConnectedAtomsOrSelf(firstAtomKey, Set.empty, bondsByAtomKeys)

    connectedAtomsOrSelf == atoms.map(_.key).toSet
  }

  private def findAllConnectedAtomsOrSelf(key: AtomKey, visited: Set[AtomKey], bondsByAtomKeys: Map[AtomKey, Seq[Bond]]): Set[AtomKey] = {
    val connectedNonVisited: Set[AtomKey] =
      bondsByAtomKeys.getOrElse(key, Seq.empty).flatMap(b => List(b.from, b.to)).filterNot(visited).filterNot(Set(key)).toSet

    if (connectedNonVisited.isEmpty) {
      visited.union(Set(key))
    } else {
      val nextVisited: Set[AtomKey] = visited.union(connectedNonVisited).union(Set(key))

      connectedNonVisited.ensuring(!_.contains(key)).foldLeft(nextVisited) { case (accVisited, ak) =>
        // Recursive call
        findAllConnectedAtomsOrSelf(ak, accVisited, bondsByAtomKeys)
      }
    }
  }
}

object LewisStructure {

  final case class AtomKey(element: ElementSymbol, seqNr: Int)

  final case class Atom(key: AtomKey, loneElectronCount: Int)

  object Atom {

    def apply(element: ElementSymbol, seqNr: Int, loneElectronCount: Int): Atom = {
      Atom(AtomKey(element, seqNr), loneElectronCount)
    }
  }

  /**
   * Bond, with a from and to, although the direction does not matter.
   */
  final class Bond(val from: AtomKey, val to: AtomKey) {
    require(from != to, s"A bond cannot connect an atom to itself")

    def touches(key: AtomKey): Boolean = key == from || key == to

    override def equals(other: Any): Boolean = other match {
      case other: Bond => Set(from, to) == Set(other.from, other.to)
      case _ => false
    }

    override def hashCode: Int = Set(from, to).hashCode

    override def toString: String = s"Bond($from, $to)"
  }

  object Bond {

    def apply(from: AtomKey, to: AtomKey): Bond = new Bond(from, to)
  }

  // Builder pattern for LewisStructure

  def builder: Builder = new Builder(Seq.empty, Seq.empty, 0)

  final case class Builder(
    val atoms: Seq[Atom],
    val bonds: Seq[Bond],
    val netCharge: Int
  ) {

    def plusAtom(key: AtomKey, loneElectronCount: Int): Builder = {
      this.copy(atoms = this.atoms.appended(Atom(key, loneElectronCount)))
    }

    def plusAtom(element: ElementSymbol, seqNr: Int, loneElectronCount: Int): Builder = {
      plusAtom(AtomKey(element, seqNr), loneElectronCount)
    }

    def plusBond(from: AtomKey, to: AtomKey): Builder = {
      this.copy(bonds = this.bonds.appended(Bond(from, to)))
    }

    def plusBonds(from: AtomKey, to: AtomKey, arity: Int): Builder = {
      1.to(arity).foldLeft(this) { case (accBuilder, n) =>
        accBuilder.plusBond(from, to)
      }
    }

    def withCharge(newCharge: Int): Builder = {
      this.copy(netCharge = newCharge)
    }

    def build: LewisStructure = new LewisStructure(atoms, bonds, netCharge)
  }

}
