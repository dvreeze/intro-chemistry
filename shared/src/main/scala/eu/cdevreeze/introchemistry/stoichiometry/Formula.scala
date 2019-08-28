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
 * A formula as used in stoichiometry. A formula may have a net positive or negative charge, so it can represent either
 * a molecule or ionic compound (which is neutral) on the one hand, or an ion (positively charged cation or negatively
 * charged anion) on the other hand.
 *
 * Example (neutral) formulas as strings (format returned by method "show" and parsed by method "parse"):
 * "O2", "CO2", "C2H4O", and "Ca3(PO4)2". A more complex example is "Ca(H2PO4)2H2O". Example ionic formula: "ion(SO4, -2)".
 *
 * The term "formula" is used as described here:
 * https://chem.libretexts.org/Courses/College_of_Marin/Marin%3A_CHEM_114_-_Introductory_Chemistry_(Daubenmire). In this
 * course, the term "formula" is also used for ions (e.g. polyatomic ions).
 *
 * For some background, see for example https://geometryofmolecules.com/formula-unit-vs-molecule/
 * and https://www.wisegeek.com/what-is-a-formula-unit.htm. See also
 * https://www.khanacademy.org/science/chemistry/atomic-structure-and-properties/introduction-to-compounds/a/paul-article-2.
 *
 * @author Chris de Vreeze
 */
sealed trait Formula {

  def isElement: Boolean

  def isCompound: Boolean

  /**
   * Returns the atom counts per element. This is also important to determine the mass of the formula.
   */
  def atomCounts: Map[ElementSymbol, Int]

  final def elementSymbols: Set[ElementSymbol] = atomCounts.keySet

  final def atomCount(elementSymbol: ElementSymbol): Int = atomCounts.getOrElse(elementSymbol, 0)

  def charge: Int

  /**
   * Returns the string representation from which the same formula can be parsed.
   */
  def show: String
}

final case class NeutralFormula(partCounts: Seq[Formula.PartQuantity]) extends Formula {
  require(partCounts.nonEmpty, s"Missing 'parts' in formula $this")

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

final case class IonicFormula(neutralFormula: NeutralFormula, charge: Int) extends Formula {
  require(charge != 0, s"The charge must not be 0")

  def isElement: Boolean = neutralFormula.isElement

  def isCompound: Boolean = neutralFormula.isCompound

  def atomCounts: Map[ElementSymbol, Int] = neutralFormula.atomCounts

  def show: String = s"ion(${neutralFormula.show}, $charge)"
}

object Formula {

  // The parts of a formula, without their quantities. I do not know if there is an official name for this.

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

  final case class CompoundPart(formula: NeutralFormula) extends Part {

    def isElement: Boolean = formula.isElement

    def atomCounts: Map[ElementSymbol, Int] = formula.atomCounts

    def show: String = s"(${formula.show})"
  }

  // A part with its quantity in a formula. I do not know if there is an official name for this.

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

  def apply(s: String): Formula = parse(s)

  def parse(s: String): Formula = {
    Parser.parseFormula(s)
  }

  object Parser {

    import fastparse._, NoWhitespace._

    def parseFormula(s: String): Formula = {
      val parseResult = fastparse.parse(s, formula(_))

      parseResult match {
        case Parsed.Success(result, _) => result
        case Parsed.Failure(_, _, _) => sys.error(s"Could not parse formula from '$s'")
      }
    }

    def formula[_: P]: P[Formula] = P(neutralFormula | ionicFormula)

    def neutralFormula[_: P]: P[NeutralFormula] =
      P(partQuantity.rep(1)).map(partQuantities => NeutralFormula(partQuantities))

    def ionicFormula[_: P]: P[IonicFormula] =
      P("ion" ~ "(" ~/ neutralFormula ~ "," ~ " ".rep(0) ~ charge ~ ")")
        .map { case (fu, charge) => IonicFormula(fu, charge) }

    def partQuantity[_: P]: P[PartQuantity] = P(part ~ count.?).map { case (p, optCnt) => PartQuantity(p, optCnt.getOrElse(1)) }

    def part[_: P]: P[Part] = P(elementPart | compoundPart)

    def elementPart[_: P]: P[ElementPart] = P(elementSymbol).map(el => ElementPart(el))

    def compoundPart[_: P]: P[CompoundPart] = P("(" ~ neutralFormula ~ ")").map(fu => CompoundPart(fu))

    def charge[_: P]: P[Int] =
      P(("+" | "-").!.? ~ count).map { case (optSign, n) => if (optSign.contains("-")) -n else n }.filter(_ != 0)

    def count[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt).filter(_ > 0)

    def elementSymbol[_: P]: P[ElementSymbol] = P((CharIn("A-Z") ~ CharIn("a-z").rep(0)).!).map(s => ElementSymbol.parse(s))
  }

  // Some well-know ionic formulas

  val Hydroxide = Formula("ion(OH, -1)")

  val Carbonate = Formula("ion(CO3, -2)")
  val Bicarbonate = Formula("ion(HCO3, -1)")

  val Nitrate = Formula("ion(NO3, -1)")
  val Nitrite = Formula("ion(NO2, -1)")

  val Sulfate = Formula("ion(SO4, -2)")
  val Sulfite = Formula("ion(SO3, -2)")

  val Phosphate = Formula("ion(PO4, -3)")

  val Cyanide = Formula("ion(CN, -1)")
  val Acetate = Formula("ion(C2H3O2, -1)")

  val Hydronium = Formula("ion(H3O, +1)")
  val Ammonium = Formula("ion(NH4, +1)")

  // Other somewhat well-known ionic formulas

  val Bromate = Formula("ion(BrO3, -1)")

  val Chlorate = Formula("ion(ClO3, -1)")
  val Perchlorate = Formula("ion(ClO4, -1)")

  val Chromate = Formula("ion(CrO4, -2)")

}
