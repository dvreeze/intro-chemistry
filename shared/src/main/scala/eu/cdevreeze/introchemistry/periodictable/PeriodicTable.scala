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
 * Periodic table, as a mapping from atomic numbers to corresponding elements.
 *
 * @author Chris de Vreeze
 */
final class PeriodicTable(val elementsByAtomicNumber: Map[Int, Element]) {
  require(
    elementsByAtomicNumber.view.mapValues(_.symbol).toMap == ElementSymbol.elementsByAtomicNumber,
    s"Mismatch between mapping from atomic numbers to element symbols and from atomic members to elements")

  def getElement(elementSymbol: ElementSymbol): Element = {
    getElementByAtomicNumber(elementSymbol.atomicNumber)
  }

  def getElementByAtomicNumber(atomicNumber: Int): Element = {
    elementsByAtomicNumber.getOrElse(atomicNumber, sys.error(s"No element found with atomic number $atomicNumber"))
  }

  def getElementByName(name: String): Element = {
    elementsByAtomicNumber.values.find(_.name.toLowerCase == name.toLowerCase)
      .getOrElse(sys.error(s"Element with name '$name' not found"))
  }

  def elementsBySymbol: Map[ElementSymbol, Element] = {
    elementsByAtomicNumber.toSeq.map(kv => ElementSymbol.fromAtomicNumber(kv._1) -> kv._2).toMap
  }
}

object PeriodicTable {

  def apply(elementsByAtomicNumber: Map[Int, Element]): PeriodicTable = {
    new PeriodicTable(elementsByAtomicNumber)
  }
}
