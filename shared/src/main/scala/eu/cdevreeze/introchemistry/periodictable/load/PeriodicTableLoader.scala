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

package eu.cdevreeze.introchemistry.periodictable.load

import scala.io.Codec

import eu.cdevreeze.introchemistry.periodictable.Element
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.periodictable.PeriodicTable
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.resolved
import eu.cdevreeze.yaidom.scalaxml.ScalaXmlElem
import eu.cdevreeze.yaidom.simple

/**
 * Periodic table loader from an XML file periodic_table.xml.
 *
 * @author Chris de Vreeze
 */
final class PeriodicTableLoader(val rootElem: simple.Elem) {

  def loadPeriodicTable(): PeriodicTable = {
    val atomicNumbers: Seq[Int] = ElementSymbol.allElements.map(_.atomicNumber).sorted

    val transformedTableElem: resolved.Elem = transformTableElement(rootElem)

    val elementsByAtomicNumber: Map[Int, Element] = atomicNumbers.map { atomicNumber =>
      val elementSymbol = ElementSymbol.fromAtomicNumber(atomicNumber)
      val element = loadElement(elementSymbol, transformedTableElem)

      atomicNumber -> element
    }.toMap

    PeriodicTable(elementsByAtomicNumber)
  }

  private def transformTableElement(tableElem: simple.Elem): resolved.Elem = {
    require(tableElem.localName == "Table", s"Not a 'Table' root element, but found instead ${tableElem.resolvedName}")

    val columnsElem = tableElem.getChildElem(_.localName == "Columns")

    val columnNames: Seq[String] = columnsElem.findAllChildElems.map(_.text.trim)
      .ensuring(_.contains("AtomicNumber")).ensuring(_.contains("Symbol")).ensuring(_.contains("AtomicMass"))

    import resolved.Node._

    emptyElem(EName("Table")).plusChildren {
      tableElem.filterChildElems(_.localName == "Row").map { rowElem =>
        val elementPropertiesElems: IndexedSeq[resolved.Elem] =
          rowElem.filterChildElems(_.localName == "Cell").zipWithIndex.map { case (cellElem, idx) =>
            require(idx < columnNames.size, s"Column index out of bounds: $idx (max. allowed ${columnNames.size - 1})")
            val name = EName(columnNames(idx))

            textElem(name, cellElem.text.trim)
          }

        emptyElem(EName("Element")).plusChildren(elementPropertiesElems)
      }
    }
  }

  private def loadElement(elementSymbol: ElementSymbol, transformedTableElem: resolved.Elem): Element = {
    val elementXmlElem: resolved.Elem =
      transformedTableElem.findElem(e => e.localName == "Element" && isXmlElemForElementSymbol(e, elementSymbol))
        .getOrElse(sys.error(s"No element $elementSymbol found"))

    val atomicNumber: Int = elementXmlElem.getChildElem(_.localName == "AtomicNumber").text.trim.toInt
    require(atomicNumber == elementSymbol.atomicNumber, s"Atomic number mismatch for element $elementSymbol")

    val name = elementXmlElem.getChildElem(_.localName == "Name").text.trim
    val atomicMass = BigDecimal(elementXmlElem.getChildElem(_.localName == "AtomicMass").text.trim)

    val electronConfig = elementXmlElem.getChildElem(_.localName == "ElectronConfiguration").text.trim
      .replace("(calculated)", "").replace("(predicted)", "").trim // Somewhat brittle!

    val electronegativityOption: Option[BigDecimal] = {
      val stringValue = elementXmlElem.getChildElem(_.localName == "Electronegativity").text.trim
      if (stringValue.isEmpty) None else Some(BigDecimal(stringValue))
    }

    val atomicRadiusOption: Option[BigDecimal] = {
      val stringValue = elementXmlElem.getChildElem(_.localName == "AtomicRadius").text.trim
      if (stringValue.isEmpty) None else Some(BigDecimal(stringValue))
    }

    val ionizationEnergyOption: Option[BigDecimal] = {
      val stringValue = elementXmlElem.getChildElem(_.localName == "IonizationEnergy").text.trim
      if (stringValue.isEmpty) None else Some(BigDecimal(stringValue))
    }

    val electronAffinityOption: Option[BigDecimal] = {
      val stringValue = elementXmlElem.getChildElem(_.localName == "ElectronAffinity").text.trim
      if (stringValue.isEmpty) None else Some(BigDecimal(stringValue))
    }

    val oxidationStates: Set[Int] =
      elementXmlElem.findChildElem(_.localName == "OxidationStates").map(_.text.trim).map(parseOxidationStates).getOrElse(Set.empty)

    Element(
      elementSymbol,
      name,
      atomicMass,
      electronConfig,
      electronegativityOption,
      atomicRadiusOption,
      ionizationEnergyOption,
      electronAffinityOption,
      oxidationStates)
  }

  private def isXmlElemForElementSymbol(elem: resolved.Elem, elementSymbol: ElementSymbol): Boolean = {
    elem.localName == "Element" && elem.findChildElem(_.localName == "Symbol").exists(_.text.trim == elementSymbol.toString)
  }

  private def parseOxidationStates(oxidationStatesString: String): Set[Int] = {
    oxidationStatesString.trim.split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
  }
}

object PeriodicTableLoader {

  def newInstance(xmlString: String): PeriodicTableLoader = {
    val scalaXmlElem: scala.xml.Elem = scala.xml.XML.loadString(xmlString)
    val simpleElem: simple.Elem = simple.Elem.from(ScalaXmlElem(scalaXmlElem))
    new PeriodicTableLoader(simpleElem)
  }

  def newInstance(): PeriodicTableLoader = {
    val is = classOf[PeriodicTableLoader].getResourceAsStream("/PubChemElements_all.xml")
    val xmlString: String = scala.io.Source.fromInputStream(is)(Codec.UTF8).mkString

    newInstance(xmlString)
  }
}
