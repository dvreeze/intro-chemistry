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
 * A formula as used in stoichiometry. A formula represents either an element or compound on the one hand, or an ion on
 * the other hand. In the former case it has no net charge, and the latter case it is a charged cation or anion. An element
 * is either an atomic element or molecular element. A compound can either represent a molecular compound or an ionic
 * compound, but this difference is invisible in formulas.
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
 * Note that in chemistry the distinction between molecular compounds (also known as covalent compounds) and ionic compounds
 * is very important, but not visible in formulas. Molecular compounds are compounds consisting of non-metals, characterized
 * by sharing of electrons. Ionic compounds, on the other hand, are compounds consisting of a metal and non-metal, characterized
 * by a transfer of electrons instead of electron sharing. Again, these 2 kinds of chemical bonds are invisible in chemical formulas.
 *
 * TODO Formulas for hydrates, like CoCl2.6H2O, which we can now only write as CoCl2(H2O)6.
 *
 * @author Chris de Vreeze
 */
sealed trait Formula {

  /**
   * Returns the atom counts per element. This is also important to determine the mass of the formula.
   */
  def atomCounts: Map[ElementSymbol, Int]

  def charge: Int

  /**
   * Returns the string representation from which the same formula can be parsed.
   */
  def show: String

  final def isSingleElement: Boolean = elementSymbols.sizeIs == 1

  final def isMultiElement: Boolean = !isSingleElement

  final def isSingleAtom: Boolean = isSingleElement && atomCounts.values.head == 1

  final def elementSymbols: Set[ElementSymbol] = atomCounts.keySet

  final def atomCount(elementSymbol: ElementSymbol): Int = atomCounts.getOrElse(elementSymbol, 0)
}

/**
 * Formula for a species that has no net charge.
 */
sealed trait NonIonFormula extends Formula {

  final def charge: Int = 0
}

/**
 * Formula for a species that has a non-zero charge, that is, an ion. So it is either a cation or an anion.
 */
sealed trait IonFormula extends Formula {

  def underlyingNonIon: NonIonFormula

  def isCation: Boolean = charge > 0

  def isAnion: Boolean = charge < 0

  final def show: String = s"ion(${underlyingNonIon.show}, $charge)"
}

sealed trait ElementFormula extends NonIonFormula {

  def elementSymbol: ElementSymbol

  def atomCount: Int

  final def atomCounts: Map[ElementSymbol, Int] = Map(elementSymbol -> atomCount)

  final def show: String = {
    val showedCount: String = if (atomCount == 1) "" else atomCount.toString
    s"$elementSymbol$showedCount"
  }
}

final case class AtomicElement(elementSymbol: ElementSymbol) extends ElementFormula {

  def atomCount: Int = 1
}

final case class MolecularElement(elementSymbol: ElementSymbol, atomCount: Int) extends ElementFormula {
  require(atomCount >= 2, s"Not a molecular element, because the atom count is $atomCount")
}

object ElementFormula {

  def apply(elementSymbol: ElementSymbol, atomCount: Int): ElementFormula = {
    if (atomCount == 1) AtomicElement(elementSymbol) else MolecularElement(elementSymbol, atomCount)
  }
}

/**
 * Either a molecular compound or an ionic compound (as opposed to an ion). In other words, any compound that has no net charge.
 * The difference between a molecular compound and an ionic compound is not visible in the formula. Hence no sub-division
 * of this type in 2 sub-types.
 */
final case class Compound(quantifiedFormulaParts: Seq[Formula.QuantifiedFormulaPart]) extends NonIonFormula {
  require(quantifiedFormulaParts.sizeIs >= 1, s"Not a compound: missing 'parts'")
  require(elementSymbols.sizeIs >= 2, s"Not a compound: only 1 element ${elementSymbols.head}")

  def atomCounts: Map[ElementSymbol, Int] = {
    Formula.getAtomCounts(quantifiedFormulaParts)
  }

  def show: String = quantifiedFormulaParts.map(_.show).mkString
}

object NonIonFormula {

  def apply(quantifiedFormulaParts: Seq[Formula.QuantifiedFormulaPart]): NonIonFormula = {
    val atomCounts: Map[ElementSymbol, Int] = Formula.getAtomCounts(quantifiedFormulaParts)

    if (atomCounts.sizeIs == 1) {
      val elementSymbol: ElementSymbol = atomCounts.head._1
      val atomCount: Int = atomCounts.head._2

      ElementFormula(elementSymbol, atomCount)
    } else {
      Compound(quantifiedFormulaParts)
    }
  }
}

final case class SingleAtomIon(elementSymbol: ElementSymbol, charge: Int) extends IonFormula {
  require(charge != 0, s"Not an ion, because the charge is 0")

  def underlyingNonIon: NonIonFormula = AtomicElement(elementSymbol)

  def atomCounts: Map[ElementSymbol, Int] = underlyingNonIon.atomCounts
}

final case class PolyAtomicIon(quantifiedFormulaParts: Seq[Formula.QuantifiedFormulaPart], charge: Int) extends IonFormula {
  require(quantifiedFormulaParts.sizeIs >= 1, s"Not a poly-atomic ion: missing 'parts'")
  require(!isSingleAtom, s"Not a poly-atomic ion: $show")
  require(charge != 0, s"Not an ion, because the charge is 0")

  def atomCounts: Map[ElementSymbol, Int] = {
    Formula.getAtomCounts(quantifiedFormulaParts)
  }

  def underlyingNonIon: NonIonFormula = {
    if (atomCounts.keySet.sizeIs == 1) {
      assert(!isSingleAtom)
      MolecularElement(atomCounts.keySet.head, atomCounts.head._2)
    } else {
      Compound(quantifiedFormulaParts)
    }
  }
}

object IonFormula {

  def apply(quantifiedFormulaParts: Seq[Formula.QuantifiedFormulaPart], charge: Int): IonFormula = {
    val atomCounts: Map[ElementSymbol, Int] = Formula.getAtomCounts(quantifiedFormulaParts)

    if (atomCounts.sizeIs == 1 && atomCounts.values.head == 1) {
      val elementSymbol: ElementSymbol = atomCounts.head._1

      SingleAtomIon(elementSymbol, charge)
    } else {
      PolyAtomicIon(quantifiedFormulaParts, charge)
    }
  }
}

object Formula {

  // The parts of a formula, without their quantities. I do not know if there is an official name for this.

  sealed trait FormulaPart {

    def atomCounts: Map[ElementSymbol, Int]

    def show: String

    final def isSingleElement: Boolean = atomCounts.keySet.sizeIs == 1

    final def isMultiElement: Boolean = !isSingleElement

    final def isSingleAtom: Boolean = isSingleElement && atomCounts.values.head == 1
  }

  final case class SingleAtomFormulaPart(elementSymbol: ElementSymbol) extends FormulaPart {

    def atomCounts: Map[ElementSymbol, Int] = Map(elementSymbol -> 1)

    def show: String = elementSymbol.toString
  }

  final case class PolyAtomicFormulaPart(quantifiedFormulaParts: Seq[QuantifiedFormulaPart]) extends FormulaPart {
    require(quantifiedFormulaParts.sizeIs >= 1, s"Not a compound: missing 'parts'")
    require(!isSingleAtom, s"Not a poly-atomic 'part': (${quantifiedFormulaParts.map(_.show).mkString})")

    def atomCounts: Map[ElementSymbol, Int] = {
      Formula.getAtomCounts(quantifiedFormulaParts)
    }

    def show: String = s"(${quantifiedFormulaParts.map(_.show).mkString})"
  }

  // A part with its quantity in a formula. I do not know if there is an official name for this.

  final case class QuantifiedFormulaPart(part: FormulaPart, count: Int) {
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

  private[stoichiometry] def getAtomCounts(quantifiedFormulaParts: Seq[QuantifiedFormulaPart]): Map[ElementSymbol, Int] = {
    quantifiedFormulaParts.foldLeft(Map.empty[ElementSymbol, Int]) { case (accAtomCounts, partQuantity) =>
      val updatedAtomCounts: Map[ElementSymbol, Int] =
        partQuantity.atomCounts.map { case (elementSymbol, count) =>
          val newCount = accAtomCounts.getOrElse(elementSymbol, 0) + count
          elementSymbol -> newCount
        }

      accAtomCounts ++ updatedAtomCounts
    }
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

    def formula[_: P]: P[Formula] = P(nonIon | ion)

    def nonIon[_: P]: P[NonIonFormula] =
      P(quantifiedFormulaPart.rep(1)).map { parts => NonIonFormula(parts) }

    def ion[_: P]: P[IonFormula] =
      P("ion" ~ "(" ~/ quantifiedFormulaPart.rep(1) ~ "," ~ " ".rep(0) ~ charge ~ ")")
        .map { case (parts, charge) => IonFormula(parts, charge) }

    def quantifiedFormulaPart[_: P]: P[QuantifiedFormulaPart] =
      P(formulaPart ~ count.?).map { case (p, optCnt) => QuantifiedFormulaPart(p, optCnt.getOrElse(1)) }

    def formulaPart[_: P]: P[FormulaPart] = P(singleAtomFormulaPart | polyAtomicFormulaPart)

    def singleAtomFormulaPart[_: P]: P[SingleAtomFormulaPart] = P(elementSymbol).map(el => SingleAtomFormulaPart(el))

    def polyAtomicFormulaPart[_: P]: P[PolyAtomicFormulaPart] =
      P("(" ~ quantifiedFormulaPart.rep(1) ~ ")").map(parts => PolyAtomicFormulaPart(parts))

    def charge[_: P]: P[Int] =
      P(("+" | "-").!.? ~ count).map { case (optSign, n) => if (optSign.contains("-")) -n else n }.filter(_ != 0)

    def count[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt).filter(_ > 0)

    def elementSymbol[_: P]: P[ElementSymbol] = P((CharIn("A-Z") ~ CharIn("a-z").rep(0)).!).map(s => ElementSymbol.parse(s))
  }

  // Some well-know ionic formulas. Also see https://www.thoughtco.com/list-of-common-polyatomic-ions-603977.

  val Hydroxide = Formula("ion(OH, -1)")

  val Carbonate = Formula("ion(CO3, -2)")
  val Bicarbonate = Formula("ion(HCO3, -1)") // also called hydrogen carbonate

  val Nitrate = Formula("ion(NO3, -1)")
  val Nitrite = Formula("ion(NO2, -1)") // Nitrite has ONE LESS oxygen than nitrate, leaving the ion charge the same

  val Sulfate = Formula("ion(SO4, -2)")
  val HydrogenSulfate = Formula("ion(HSO4, -1)") // also called bisulfate
  val Sulfite = Formula("ion(SO3, -2)") // Sulfite has ONE LESS oxygen than sulfate, leaving the ion charge the same
  val Thiosulfate = Formula("ion(S2O3, -2)")

  val Phosphate = Formula("ion(PO4, -3)")
  val HydrogenPhosphate = Formula("ion(HPO4, -2)")
  val DihydrogenPhosphate = Formula("ion(H2PO4, -1)")

  val Cyanide = Formula("ion(CN, -1)")
  val Acetate = Formula("ion(C2H3O2, -1)") // Could also be written as: ion(CH3CO2, -1)

  val Hydronium = Formula("ion(H3O, +1)")
  val Ammonium = Formula("ion(NH4, +1)")

  // Other somewhat well-known ionic formulas

  val Bromate = Formula("ion(BrO3, -1)")

  val Chlorate = Formula("ion(ClO3, -1)")
  val Perchlorate = Formula("ion(ClO4, -1)") // Perchlorate has ONE MORE oxygen than chlorate, leaving the charge the same
  val Chlorite = Formula("ion(ClO2, -1)") // Chlorite has ONE LESS oxygen than chlorate, leaving the ion charge the same
  val Hypochlorite = Formula("ion(ClO, -1)") // Hypochlorite has TWO LESS oxygen than chlorate, leaving the ion charge the same

  val Chromate = Formula("ion(CrO4, -2)")
  val Dichromate = Formula("ion(Cr2O7, -2)")

  val Permanganate = Formula("ion(MnO4, -1)")

  val Peroxide = Formula("ion(O2, -2)") // Peroxide has ONE MORE oxygen than oxide, leaving the charge the same.

  val Cyanate = Formula("ion(OCN, -1)")
  val Thiocyanate = Formula("ion(SCN, -1)")

  val Borate = Formula("ion(BO3, -3)")

  val WellKnownPolyAtomicIons: Map[String, Formula] = {
    Map(
      "hydroxide" -> Hydroxide,
      "carbonate" -> Carbonate,
      "bicarbonate" -> Bicarbonate,
      "nitrate" -> Nitrate,
      "nitrite" -> Nitrite,
      "sulfate" -> Sulfate,
      "hydrogen sulfate" -> HydrogenSulfate,
      "sulfite" -> Sulfite,
      "thiosulfate" -> Thiosulfate,
      "phosphate" -> Phosphate,
      "hydrogen phosphate" -> HydrogenPhosphate,
      "dihydrogen phosphate" -> DihydrogenPhosphate,
      "cyanide" -> Cyanide,
      "cyanate" -> Cyanate,
      "thiocyanate" -> Thiocyanate,
      "acetate" -> Acetate,
      "hydronium" -> Hydronium,
      "ammonium" -> Ammonium,
      "bromate" -> Bromate,
      "chlorate" -> Chlorate,
      "perchlorate" -> Perchlorate,
      "chlorite" -> Chlorite,
      "hypochlorite" -> Hypochlorite,
      "chromate" -> Chromate,
      "dichromate" -> Dichromate,
      "permangate" -> Permanganate,
      "peroxide" -> Peroxide, // poly-atomic, but single-element, and in that sense different from the other poly-atomic ions!
      "borate" -> Borate,
    )
  }
}
