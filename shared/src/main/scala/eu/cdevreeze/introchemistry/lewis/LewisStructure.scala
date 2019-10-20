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

import eu.cdevreeze.introchemistry.internal.UndirectedGraph
import eu.cdevreeze.introchemistry.internal.UndirectedGraph.Edge
import eu.cdevreeze.introchemistry.lewis.LewisStructure.Atom
import eu.cdevreeze.introchemistry.lewis.LewisStructure.AtomKey
import eu.cdevreeze.introchemistry.lewis.LewisStructure.Bond
import eu.cdevreeze.introchemistry.lewis.LewisStructureApi.AbstractLewisStructure
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * Representation of a Lewis structure. This class depends on the AufbauPrinciple object, and therefore on the orbitals Scala package.
 *
 * @author Chris de Vreeze
 */
final class LewisStructure(
  val atoms: Seq[Atom],
  val bonds: Seq[Bond],
  val netCharge: Int) extends AbstractLewisStructure {

  type This = LewisStructure

  require(atoms.nonEmpty, s"Expected at least one atom")
  require(atoms.size == atoms.map(_.key).distinct.size, s"Duplicate atom keys not allowed")

  require(
    bonds.flatMap(b => List(b.from, b.to)).toSet.subsetOf(atoms.map(_.key).toSet),
    s"Corrupt Lewis structure, because not all bonds refer to atoms in the Lewis structure")

  require(
    netCharge == atomKeys.map(getFormalCharge).sum,
    s"The formal charges of the atoms do not add up to the net charge of the Lewis structure")

  /**
   * Returns true if both atoms (that must occur in this Lewis structure) can share (currently lone) electrons.
   * The only criterion used is the availability of at least one lone electron in boty atoms.
   * It is assumed that the Lewis structure is covalent, without looking at electronegativity differences between the 2 atoms.
   */
  def canMakeBond(atomKey1: AtomKey, atomKey2: AtomKey): Boolean = {
    require(atomKeys.contains(atomKey1), s"Atom $atomKey1 is not part of Lewis structure $this")
    require(atomKeys.contains(atomKey2), s"Atom $atomKey2 is not part of Lewis structure $this")

    getAtom(atomKey1).loneElectronCount >= 1 && getAtom(atomKey2).loneElectronCount >= 1
  }

  def makeBond(atomKey1: AtomKey, atomKey2: AtomKey): LewisStructure = {
    require(canMakeBond(atomKey1, atomKey2), s"Cannot make a bond between atoms $atomKey1 and $atomKey2 (missing currently lone electrons)")

    LewisStructure(
      atomKeys.map { atomKey =>
        atomKey match {
          case `atomKey1` => getAtom(atomKey1).minusLoneElectron
          case `atomKey2` => getAtom(atomKey2).minusLoneElectron
          case atomKey => getAtom(atomKey)
        }
      },
      bonds.appended(Bond(atomKey1, atomKey2)),
      netCharge
    ).ensuring(_.electronCount == this.electronCount)
  }

  def makeDoubleBond(atomKey1: AtomKey, atomKey2: AtomKey): LewisStructure = {
    makeBond(atomKey1, atomKey2).makeBond(atomKey1, atomKey2)
  }

  def makeTripleBond(atomKey1: AtomKey, atomKey2: AtomKey): LewisStructure = {
    makeBond(atomKey1, atomKey2).makeBond(atomKey1, atomKey2).makeBond(atomKey1, atomKey2)
  }

  def getUnderlyingUndirectedGraph: UndirectedGraph[AtomKey] = {
    UndirectedGraph.fromVertices(atomKeys.toSet).plusEdges(bonds.map(b => Edge(b.from, b.to)).toSet)
  }
}

object LewisStructure {

  def apply(atoms: Seq[Atom], bonds: Seq[Bond], netCharge: Int): LewisStructure = new LewisStructure(atoms, bonds, netCharge)

  /**
   * "Extension method" to turn an ElementSymbol with (typically one-based) index number into an AtomKey.
   */
  implicit class ToAtomKey(element: ElementSymbol) {

    def at(i: Int): AtomKey = AtomKey(element, i)
  }

  final case class AtomKey(element: ElementSymbol, seqNr: Int)

  final case class Atom(key: AtomKey, loneElectronCount: Int) {
    require(loneElectronCount >= 0, s"Negative lone electron count not allowed")

    def minusLoneElectron: Atom = Atom(key, loneElectronCount - 1)
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
    val netCharge: Int) extends AbstractLewisStructure {

    type This = Builder

    def plusRawAtom(key: AtomKey): Builder = {
      plusAtom(key, 0)
    }

    def plusAtom(key: AtomKey, loneElectronCount: Int): Builder = {
      this.copy(atoms = this.atoms.appended(Atom(key, loneElectronCount)))
    }

    def plusBond(from: AtomKey, to: AtomKey): Builder = {
      this.copy(bonds = this.bonds.appended(Bond(from, to)))
    }

    def plusBonds(from: AtomKey, to: AtomKey, arity: Int): Builder = {
      require(arity >= 1, s"Arity must be at least 1")

      1.to(arity).foldLeft(this) { case (accBuilder, n) =>
        accBuilder.plusBond(from, to)
      }
    }

    def withCharge(newCharge: Int): Builder = {
      this.copy(netCharge = newCharge)
    }

    def updateLoneElectronCount(key: AtomKey, f: Int => Int): Builder = {
      this.copy(atoms = this.atoms.map { atom =>
        if (atom.key == key) atom.copy(loneElectronCount = f(atom.loneElectronCount)) else atom
      })
    }

    def incrementLoneElectronCount(key: AtomKey, amount: Int): Builder = {
      updateLoneElectronCount(key, _ + amount)
    }

    def getUnderlyingUndirectedGraph: UndirectedGraph[AtomKey] = {
      UndirectedGraph.fromVertices(atomKeys.toSet).plusEdges(bonds.map(b => Edge(b.from, b.to)).toSet)
    }

    def build: LewisStructure = new LewisStructure(atoms, bonds, netCharge)
  }

}
