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
 * An element in the periodic table, containing the element symbol as well as much other data as found in periodic tables.
 *
 * @author Chris de Vreeze
 */
final case class Element(
  symbol: ElementSymbol,
  name: String,
  atomicMass: BigDecimal,
  oxidationStates: Set[Int]) {

  def symbolName: String = symbol.symbolName

  def atomicNumber: Int = symbol.atomicNumber

  def chemicalGroup: ChemicalGroupBlock = symbol.chemicalGroup

  override def toString: String = {
    (0 to productArity).map(idx => s"${productElementName(idx) -> productElement(idx)}").mkString("[ ", ", ", " ]")
  }
}
