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
 * A formula unit as used in stoichiometry. A formula unit may have a positive or negative charge.
 *
 * Example (neutral) formula units as strings (format returned by method "show" and parsed by method "parse"):
 * "O2", "CO2", "C2H4O", and "Ca3(PO4)2". A more complex example is "Ca(H2PO4)2H2O". Example ionic formula unit: "ion(SO4, -2)".
 *
 * Note that a formula unit is not the same as a molecule. See for example https://geometryofmolecules.com/formula-unit-vs-molecule/
 * and https://www.wisegeek.com/what-is-a-formula-unit.htm. See also
 * https://www.khanacademy.org/science/chemistry/atomic-structure-and-properties/introduction-to-compounds/a/paul-article-2.
 *
 * @author Chris de Vreeze
 */
sealed trait FormulaUnit {

  def isElement: Boolean

  def isCompound: Boolean

  /**
   * Returns the atom counts per element. This is important to determine the mass of the formula unit.
   * It is also important to determine the elements, if the quantities are ignored.
   */
  def atomCounts: Map[ElementSymbol, Int]

  def charge: Int

  /**
   * Returns the string representation from which the same formula unit can be parsed.
   */
  def show: String
}

final case class NeutralFormulaUnit(partCounts: Seq[FormulaUnit.PartQuantity]) extends FormulaUnit {
  require(partCounts.nonEmpty, s"Missing 'parts' in formula unit $this")

  def isElement: Boolean = atomCounts.keySet.sizeIs == 1

  def isCompound: Boolean = !isElement

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

  def charge: Int = 0

  def show: String = partCounts.map(_.show).mkString
}

final case class IonicFormulaUnit(neutralFormulaUnit: NeutralFormulaUnit, charge: Int) extends FormulaUnit {
  require(charge != 0, s"The charge must not be 0")

  def isElement: Boolean = neutralFormulaUnit.isElement

  def isCompound: Boolean = neutralFormulaUnit.isCompound

  def atomCounts: Map[ElementSymbol, Int] = neutralFormulaUnit.atomCounts

  def show: String = s"ion(${neutralFormulaUnit.show}, $charge)"
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

  final case class CompoundPart(formulaUnit: NeutralFormulaUnit) extends Part {

    def isElement: Boolean = formulaUnit.isElement

    def atomCounts: Map[ElementSymbol, Int] = formulaUnit.atomCounts

    def show: String = s"(${formulaUnit.show})"
  }

  // A part with its quantity in a formula unit. I do not know if there is an official name for this.

  final case class PartQuantity(part: Part, count: Int) {
    require(count >= 1, s"Expected count at least 1 of 'part' $part")

    def atomCounts: Map[ElementSymbol, Int] = {
      part.atomCounts.view.mapValues(_ * count).toMap
    }

    def show: String = {
      val showedCount: String = if (count == 1) "" else count.toString
      s"${part.show}$showedCount"
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

    def formulaUnit[_: P]: P[FormulaUnit] = P(neutralFormulaUnit | ionicFormulaUnit)

    def neutralFormulaUnit[_: P]: P[NeutralFormulaUnit] =
      P(partQuantity.rep(1)).map(partQuantities => NeutralFormulaUnit(partQuantities))

    def ionicFormulaUnit[_: P]: P[IonicFormulaUnit] =
      P("ion" ~ "(" ~ neutralFormulaUnit ~ "," ~ " ".rep(0) ~ charge ~ ")")
        .map { case (fu, charge) => IonicFormulaUnit(fu, charge) }

    def partQuantity[_: P]: P[PartQuantity] = P(part ~ count.?).map { case (p, optCnt) => PartQuantity(p, optCnt.getOrElse(1)) }

    def part[_: P]: P[Part] = P(elementPart | compoundPart)

    def elementPart[_: P]: P[ElementPart] = P(elementSymbol).map(el => ElementPart(el))

    def compoundPart[_: P]: P[CompoundPart] = P("(" ~ neutralFormulaUnit ~ ")").map(fu => CompoundPart(fu))

    def charge[_: P]: P[Int] =
      P(("+" | "-").!.? ~ count).map { case (optSign, n) => if (optSign.contains("-")) -n else n }.filter(_ != 0)

    def count[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt).filter(_ != 0)

    def elementSymbol[_: P]: P[ElementSymbol] = P((CharIn("A-Z") ~ CharIn("a-z").rep(0)).!).map(s => ElementSymbol.parse(s))
  }

  // Some well-know ionic formula units

  val Hydroxide = FormulaUnit("ion(OH, -1)")
  val Carbonate = FormulaUnit("ion(CO3, -2)")
  val Bicarbonate = FormulaUnit("ion(HCO3, -1)")
  val Nitrate = FormulaUnit("ion(NO3, -1)")
  val Nitrite = FormulaUnit("ion(NO2, -1)")
  val Sulfate = FormulaUnit("ion(SO4, -2)")
  val Sulfite = FormulaUnit("ion(SO3, -2)")
  val Phosphate = FormulaUnit("ion(PO4, -3)")
  val Cyanide = FormulaUnit("ion(CN, -1)")
  val Acetate = FormulaUnit("ion(C2H3O2, -1)")
  val Hydronium = FormulaUnit("ion(H3O, +1)")
  val Ammonium = FormulaUnit("ion(NH4, +1)")

}
