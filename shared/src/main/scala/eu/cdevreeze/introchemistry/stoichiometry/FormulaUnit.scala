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

package eu.cdevreeze.introchemistry.stoichiometry

import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * A formula unit as used in stoichiometry. No charge is expressed in the formula unit.
 *
 * Example formula units as strings (format returned by method "show" and parsed by method "parse"):
 * "CO2", "C2H4O", and "Ca3(PO4)2". A more complex example is "Ca(H2PO4)2H2O".
 *
 * Note that a formula unit is not the same as a molecule. See for example https://geometryofmolecules.com/formula-unit-vs-molecule/
 * and https://www.wisegeek.com/what-is-a-formula-unit.htm. See also
 * https://www.khanacademy.org/science/chemistry/atomic-structure-and-properties/introduction-to-compounds/a/paul-article-2.
 *
 * @author Chris de Vreeze
 */
final case class FormulaUnit(partCounts: Seq[FormulaUnit.PartQuantity]) {
  require(partCounts.nonEmpty, s"Missing 'parts' in formula unit $this")

  def isElement: Boolean = {
    partCounts == 1 && partCounts.head.part.isElement
  }

  def isCompound: Boolean = !isElement

  /**
   * Returns the atom counts per element. This is important to determine the mass of the formula unit.
   * It is also important to determine the elements, if the quantities are ignored.
   */
  def atomCounts: Map[ElementSymbol, Int] = {
    partCounts.foldLeft(Map.empty[ElementSymbol, Int]) { case (accAtomCounts, partQuantity) =>
      val updatedAtomCounts: Map[ElementSymbol, Int] =
        partQuantity.atomCounts.map { case (elementSymbol, count) =>
          val newCount = accAtomCounts.getOrElse(elementSymbol, 0) + count
          elementSymbol -> newCount
        }

      accAtomCounts ++ updatedAtomCounts
    }
  }

  def show: String = partCounts.mkString
}

object FormulaUnit {

  // The parts of a formula unit, without their quantities. I do not know if there is an official name for this.

  sealed trait Part {

    def isElement: Boolean

    def atomCounts: Map[ElementSymbol, Int]

    def show: String
  }

  final case class ElementPart(elementSymbol: ElementSymbol) extends Part {

    def isElement: Boolean = true

    def atomCounts: Map[ElementSymbol, Int] = Map(elementSymbol -> 1)

    def show: String = elementSymbol.toString
  }

  final case class CompoundPart(formulaUnit: FormulaUnit) extends Part {

    def isElement: Boolean = formulaUnit.isElement

    def atomCounts: Map[ElementSymbol, Int] = formulaUnit.atomCounts

    def show: String = s"($formulaUnit)"
  }

  // A part with its quantity in a formula unit. I do not know if there is an official name for this.

  final case class PartQuantity(part: Part, count: Int) {
    require(count >= 1, s"Expected count at least 1 of 'part' $part")

    def atomCounts: Map[ElementSymbol, Int] = {
      part.atomCounts.view.mapValues(_ * count).toMap
    }

    def show: String = {
      val showedCount: String = if (count == 1) "" else count.toString
      s"$part$showedCount"
    }
  }

  def apply(s: String): FormulaUnit = parse(s)

  def parse(s: String): FormulaUnit = {
    Parser.parseFormulaUnit(s)
  }

  object Parser {

    import fastparse._, NoWhitespace._

    def parseFormulaUnit(s: String): FormulaUnit = {
      val parseResult = fastparse.parse(s, formulaUnit(_))

      parseResult match {
        case Parsed.Success(result, _) => result
        case Parsed.Failure(_, _, _) => sys.error(s"Could not parse formula unit from '$s'")
      }
    }

    def formulaUnit[_: P]: P[FormulaUnit] = P( partQuantity.rep(1) ).map(partQuantities => FormulaUnit(partQuantities))

    def partQuantity[_: P]: P[PartQuantity] = P( part ~ count.? ).map { case (p, optCnt) => PartQuantity(p, optCnt.getOrElse(1)) }

    def part[_: P]: P[Part] = P( elementPart | compoundPart )

    def elementPart[_: P]: P[ElementPart] = P( elementSymbol ).map(el => ElementPart(el))

    def compoundPart[_: P]: P[CompoundPart] = P( "(" ~ formulaUnit ~ ")" ).map(fu => CompoundPart(fu))

    def count[_: P]: P[Int] = P( CharIn("0-9").rep(1).! ).map(_.toInt)

    def elementSymbol[_: P]: P[ElementSymbol] = P( (CharIn("A-Z") ~ CharIn("a-z").rep(0)).! ).map(s => ElementSymbol.parse(s))
  }
}
