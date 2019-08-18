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

package eu.cdevreeze.introchemistry.periodictable.jvm

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import eu.cdevreeze.introchemistry.periodictable.Element
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.periodictable.PeriodicTable

/**
 * Periodic table loader on the JVM.
 *
 * @author Chris de Vreeze
 */
final class PeriodicTableLoader(config: Config) {

  def loadPeriodicTable(): PeriodicTable = {
    val atomicNumbers: Seq[Int] = ElementSymbol.allElements.map(_.atomicNumber).sorted

    val elementsByAtomicNumber: Map[Int, Element] = atomicNumbers.map { atomicNumber =>
      val elementSymbol = ElementSymbol.fromAtomicNumber(atomicNumber)
      val element = loadElement(elementSymbol)

      atomicNumber -> element
    }.toMap

    PeriodicTable(elementsByAtomicNumber)
  }

  private def loadElement(elementSymbol: ElementSymbol): Element = {
    val elementConfig = config.getConfig(elementSymbol.symbolName.toLowerCase)

    val atomicNumber = elementConfig.getInt("atomicNumber")
    require(atomicNumber == elementSymbol.atomicNumber, s"Atomic number mismatch for element $elementSymbol")
    val name = elementConfig.getString("name")
    val atomicMass = BigDecimal(elementConfig.getString("atomicMass"))

    Element(elementSymbol, name, atomicMass)
  }
}

object PeriodicTableLoader {

  def newInstance(): PeriodicTableLoader = {
    val config = ConfigFactory.load()
    new PeriodicTableLoader(config)
  }
}
