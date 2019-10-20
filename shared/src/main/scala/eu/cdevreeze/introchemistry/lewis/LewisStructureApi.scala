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
import eu.cdevreeze.introchemistry.lewis.LewisStructure.Atom
import eu.cdevreeze.introchemistry.lewis.LewisStructure.AtomKey
import eu.cdevreeze.introchemistry.lewis.LewisStructure.Bond
import eu.cdevreeze.introchemistry.orbitals.AufbauPrinciple
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * Abstract API of the representation of a Lewis structure. It is shared by class LewisStructure and its Builder.
 *
 * @author Chris de Vreeze
 */
trait LewisStructureApi {

  type This <: LewisStructureApi

  def atoms: Seq[Atom]

  def bonds: Seq[Bond]

  def netCharge: Int

  def atomKeys: Seq[AtomKey]

  /**
   * Returns the atom counts per element. It can be matched against the atom counts of a corresponding formula.
   */
  def atomCounts: Map[ElementSymbol, Int]

  def getAtom(key: AtomKey): Atom

  def findBonds(key: AtomKey): Seq[Bond]

  def getElectronCount(key: AtomKey): Int

  def electronCount: Int

  /**
   * Returns a count of the own and shared electrons of the atom. Typically the result should be 8, as per the octet rule.
   */
  def getSurroundingElectronCount(key: AtomKey): Int

  def obeysOctetRule(key: AtomKey): Boolean

  /**
   * Returns the formal charge of the given atom, which is the valence electron count of the element, minus the electron
   * count of this atom in the Lewis structure (lone or bound). Ideally it is 0, and otherwise it is typically as close to 0 as possible
   * in the Lewis structure.
   */
  def getFormalCharge(key: AtomKey): Int

  /**
   * Returns a mapping from all atom keys to their formal charges in the Lewis structure.
   */
  def computeFormalChargeMapping: Map[AtomKey, Int]

  /**
   * A Lewis structure must be a connected undirected graph, ignoring double bonds etc. This function returns true if that is indeed the case.
   */
  def underlyingGraphIsConnected: Boolean

  /**
   * Returns true if the underlying undirected graph (ignoring double bonds etc.) is a tree. That is, returns true if the
   * underlying undirected graph is a connected acyclic undirected graph.
   */
  def underlyingGraphIsTree: Boolean

  def getUnderlyingUndirectedGraph: UndirectedGraph[AtomKey]
}

object LewisStructureApi {

  abstract class AbstractLewisStructure extends LewisStructureApi {

    type This <: AbstractLewisStructure

    def atomKeys: Seq[AtomKey] = atoms.map(_.key)

    def atomCounts: Map[ElementSymbol, Int] = atoms.groupBy(_.key.element).view.mapValues(_.size).toMap

    def getAtom(key: AtomKey): Atom = atoms.find(_.key == key).getOrElse(sys.error(s"Could not find atom $key"))

    def findBonds(key: AtomKey): Seq[Bond] = bonds.filter(_.touches(key))

    def getElectronCount(key: AtomKey): Int = getAtom(key).loneElectronCount + findBonds(key).size

    def electronCount: Int = atoms.map(atom => getElectronCount(atom.key)).sum

    def getSurroundingElectronCount(key: AtomKey): Int = getAtom(key).loneElectronCount + 2 * findBonds(key).size

    def obeysOctetRule(key: AtomKey): Boolean = getSurroundingElectronCount(key) == 8

    /**
     * Returns the formal charge of the given atom, which is the valence electron count of the element, minus the electron
     * count of this atom in the Lewis structure (lone or bound). Ideally it is 0, and otherwise it is typically as close to 0 as possible
     * in the Lewis structure.
     */
    def getFormalCharge(key: AtomKey): Int = {
      AufbauPrinciple.getValenceElectronCount(key.element) - getElectronCount(key)
    }

    /**
     * Returns a mapping from all atom keys to their formal charges in the Lewis structure.
     */
    def computeFormalChargeMapping: Map[AtomKey, Int] = {
      atomKeys.map(key => key -> getFormalCharge(key)).toMap
    }

    /**
     * A Lewis structure must be a connected undirected graph, ignoring double bonds etc. This function returns true if that is indeed the case.
     */
    def underlyingGraphIsConnected: Boolean = getUnderlyingUndirectedGraph.isConnectedGraph

    /**
     * Returns true if the underlying undirected graph (ignoring double bonds etc.) is a tree. That is, returns true if the
     * underlying undirected graph is a connected acyclic undirected graph.
     */
    def underlyingGraphIsTree: Boolean = getUnderlyingUndirectedGraph.isTree
  }
}
