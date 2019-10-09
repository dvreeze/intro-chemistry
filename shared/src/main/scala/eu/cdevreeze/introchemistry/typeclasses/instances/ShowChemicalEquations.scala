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

package eu.cdevreeze.introchemistry.typeclasses.instances

import eu.cdevreeze.introchemistry.periodictable.Element
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation.FormulaQuantity
import eu.cdevreeze.introchemistry.stoichiometry.Compound
import eu.cdevreeze.introchemistry.stoichiometry.ElementFormula
import eu.cdevreeze.introchemistry.stoichiometry.Formula
import eu.cdevreeze.introchemistry.stoichiometry.Formula.FormulaPart
import eu.cdevreeze.introchemistry.stoichiometry.Formula.MonatomicFormulaPart
import eu.cdevreeze.introchemistry.stoichiometry.Formula.PolyatomicFormulaPart
import eu.cdevreeze.introchemistry.stoichiometry.Formula.QuantifiedFormulaPart
import eu.cdevreeze.introchemistry.stoichiometry.IonFormula
import eu.cdevreeze.introchemistry.stoichiometry.NonIonFormula
import eu.cdevreeze.introchemistry.typeclasses.Show

/**
 * Holder of instances of type class Show for Element, Formula, ChemicalEquation etc. Inspired by Cats.
 *
 * @author Chris de Vreeze
 */
object ShowChemicalEquations {

  // Below, only a few type class instances are implicit vals, for more predictable implicit resolution.

  // Show type class instance for Element

  implicit val showElement: Show[Element] = Show.show { value: Element =>
    (0 until value.productArity).map { idx =>
      val propertyName = value.productElementName(idx)

      val propertyValue = propertyName match {
        case "electronegativityOption" | "atomicRadiusOption" | "ionizationEnergyOption" | "electronAffinityOption" =>
          value.productElement(idx).asInstanceOf[Option[_]].map(_.toString).getOrElse("")
        case "oxidationStates" => value.productElement(idx).asInstanceOf[Set[_]].mkString("[", ",", "]")
        case _ => value.productElement(idx)
      }

      s"$propertyName -> $propertyValue"
    }.mkString("[ ", ", ", " ]")
  }

  // Show type class instances for Formula and sub-types

  implicit val showFormula: Show[Formula] = Show.show { value: Formula =>
    value match {
      case value: NonIonFormula => showNonIonFormula.show(value)
      case value: IonFormula => showIonFormula.show(value)
    }
  }

  val showNonIonFormula: Show[NonIonFormula] = Show.show { value: NonIonFormula =>
    value match {
      case value: ElementFormula => showElementFormula.show(value)
      case value: Compound => showCompound.show(value)
    }
  }

  val showIonFormula: Show[IonFormula] = Show.show { value: IonFormula =>
    s"${showNonIonFormula.show(value.underlyingNonIon)}{${value.charge}}"
  }

  val showElementFormula: Show[ElementFormula] = Show.show { value: ElementFormula =>
    val showedCount: String = if (value.atomCount == 1) "" else value.atomCount.toString
    s"${value.elementSymbol}$showedCount"
  }

  val showCompound: Show[Compound] = Show.show { value: Compound =>
    value.quantifiedFormulaParts.map(showQuantifiedFormulaPart.show).mkString
  }

  // Show type class instances for FormulaPart and sub-types

  val showFormulaPart: Show[FormulaPart] = Show.show { value: FormulaPart =>
    value match {
      case value: MonatomicFormulaPart => showMonatomicFormulaPart.show(value)
      case value: PolyatomicFormulaPart => showPolyatomicFormulaPart.show(value)
    }
  }

  val showMonatomicFormulaPart: Show[MonatomicFormulaPart] = Show.show { value: MonatomicFormulaPart =>
    value.elementSymbol.toString
  }

  val showPolyatomicFormulaPart: Show[PolyatomicFormulaPart] = Show.show { value: PolyatomicFormulaPart =>
    s"(${value.quantifiedFormulaParts.map(showQuantifiedFormulaPart.show).mkString})"
  }

  // Show type class instance for QuantifiedFormulaPart

  val showQuantifiedFormulaPart: Show[QuantifiedFormulaPart] = Show.show { value: QuantifiedFormulaPart =>
    val showedCount: String = if (value.count == 1) "" else value.count.toString
    s"${showFormulaPart.show(value.part)}$showedCount"
  }

  // Show type class instance for ChemicalEquation

  implicit val showChemicalEquation: Show[ChemicalEquation] = Show.show { value: ChemicalEquation =>
    s"${value.reactants.map(showFormulaQuantity.show).mkString(" + ")} = ${value.products.map(showFormulaQuantity.show).mkString(" + ")}"
  }

  // Show type class instance for FormulaQuantity

  val showFormulaQuantity: Show[FormulaQuantity] = Show.show { value: FormulaQuantity =>
    val quantityString = if (value.quantity == 1) "" else value.quantity.toString + " "
    val phaseString = value.phaseOption.map(ph => s"($ph)").getOrElse("")

    s"$quantityString${showFormula.show(value.formula)} $phaseString".trim
  }
}
