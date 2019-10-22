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

package eu.cdevreeze.introchemistry.api

import eu.cdevreeze.introchemistry.orbitals.ElectronConfig
import eu.cdevreeze.introchemistry.periodictable.Element
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.periodictable.load.PeriodicTableLoader
import eu.cdevreeze.introchemistry.stoichiometry.AtomicElement
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation
import eu.cdevreeze.introchemistry.stoichiometry.ElementFormula
import eu.cdevreeze.introchemistry.stoichiometry.Formula
import org.scalatest.funsuite.AnyFunSuite

/**
 * Test of consistency of query API.
 *
 * @author Chris de Vreeze
 */
class QueryApiTest extends AnyFunSuite {

  import SimpleQueryApi._

  test("testQueryElements") {
    val queryApi: SimpleQueryApi = SimpleQueryApi(PeriodicTableLoader.newInstance().loadPeriodicTable())

    val elementMap: Map[ElementSymbol, Element] = ElementSymbol.allElements.map(e => e -> queryApi.getElement(e)).toMap

    assertResult(true) {
      elementMap.forall { case (elemSymbol, elem) => elemSymbol == elem.symbol }
    }

    assertResult(true) {
      elementMap.forall { case (elemSymbol, elem) => elemSymbol.atomicNumber == elem.atomicNumber }
    }

    assertResult(true) {
      elementMap.forall { case (elemSymbol, elem) => elem == queryApi.getElementByAtomicNumber(elemSymbol.atomicNumber) }
    }

    assertResult(true) {
      elementMap.forall { case (elemSymbol, elem) => elem == queryApi.getElementByAtomicNumber(elem.atomicNumber) }
    }

    assertResult(true) {
      elementMap.forall { case (elemSymbol, elem) => elem == queryApi.getElementByName(elem.name) }
    }
  }

  test("testQueryMass") {
    val queryApi: SimpleQueryApi = SimpleQueryApi(PeriodicTableLoader.newInstance().loadPeriodicTable())

    // Amu and gram per mole are interchangeable

    assertResult(true) {
      ElementSymbol.allElements.forall(e => queryApi.massOfAtomInAmu(e) == queryApi.massOfAtomInGramPerMole(e))
    }

    assertResult(true) {
      ElementSymbol.allElements.forall(e => queryApi.massOfAtomInAmu(e) == queryApi.massInAmu(AtomicElement(e)))
    }

    assertResult(true) {
      someFormulas.forall(f => queryApi.massInAmu(f) == queryApi.massInGramPerMole(f))
    }

    assertResult(true) {
      someFormulas.forall { f =>
        queryApi.massInAmu(f) == f.atomCounts.toSeq.map { case (atom, count) => count * queryApi.massOfAtomInAmu(atom) }.sum
      }
    }
  }

  test("testBalancingEquations") {
    val queryApi: SimpleQueryApi = SimpleQueryApi(PeriodicTableLoader.newInstance().loadPeriodicTable())

    assertResult(someEquations.map(Some(_))) {
      someRawEquations.map(ce => queryApi.tryToBalanceChemicalEquation(ce))
    }

    assertResult(true) {
      someRawEquations.map(ce => queryApi.tryToBalanceChemicalEquation(ce)).forall(_.exists(_.isBalanced))
    }
  }

  test("testElectronCount") {
    val queryApi: SimpleQueryApi = SimpleQueryApi(PeriodicTableLoader.newInstance().loadPeriodicTable())

    val absoluteElectronConfigMap: Map[ElementSymbol, ElectronConfig] =
      ElementSymbol.allElements.map(e => e -> queryApi.getAbsoluteElectronConfig(e)).toMap

    assertResult(true) {
      absoluteElectronConfigMap.forall(_._2.isAbsolute)
    }

    assertResult(true) {
      absoluteElectronConfigMap.forall { case (elemSymbol, electronConfig) =>
        electronConfig == queryApi.getAbsoluteElectronConfig(queryApi.getElectronConfig(elemSymbol))
      }
    }

    // The number of electrons follows easily from the absolute electron config

    assertResult(true) {
      absoluteElectronConfigMap.forall { case (elemSymbol, electronConfig) =>
        queryApi.getElectronCount(elemSymbol) == electronConfig.subshellConfigs.map(_.electronCount).sum
      }
    }

    // The number of protons (the atomic number) and the number of electrons are the same in an atomic element

    assertResult(true) {
      absoluteElectronConfigMap.forall { case (elemSymbol, electronConfig) => elemSymbol.atomicNumber == electronConfig.relativeElectronCount }
    }
  }

  test("testValenceElectronCount") {
    val queryApi: SimpleQueryApi = SimpleQueryApi(PeriodicTableLoader.newInstance().loadPeriodicTable())

    val absoluteElectronConfigMap: Map[ElementSymbol, ElectronConfig] =
      ElementSymbol.allElements.map(e => e -> queryApi.getAbsoluteElectronConfig(e)).toMap

    // The valence electron count follows easily from the electron configuration

    assertResult(true) {
      absoluteElectronConfigMap.forall { case (elemSymbol, electronConfig) =>
        val maxLevel: Int = electronConfig.subshellConfigs.map(_.level).max

        queryApi.getValenceElectronCount(elemSymbol) == electronConfig.subshellConfigs.filter(_.level == maxLevel).map(_.electronCount).sum
      }
    }

    assertResult(true) {
      someFormulas.forall { f =>
        queryApi.getValenceElectronCount(f) ==
          f.atomCounts.map { case (atom: ElementSymbol, count: Int) => count * queryApi.getValenceElectronCount(atom) }.sum - f.charge
      }
    }
  }

  private def someRawEquations: Seq[ChemicalEquation] = Seq(
    "NaBr + F2 = NaF + Br2",
    "K + H2O = KOH + H2",
    "H2O2 = H2O + O2",
    "CuSO4 + KCN = Cu(CN)2 + K2SO4",
    "P4 + O2 = P4O6",
    "CH4 + O2 = CO2 + H2O",
    "N2 + F2 = NF3",
    "AlBr3 + K2SO4 = KBr + Al2(SO4)3",
    "K + Cl2 = KCl",
    "Al (s) + HCl (aq) = H2 (g) + AlCl3 (aq)",
    "N2 (g) + H2 (g) = NH3 (g)",
    "C2H6 + O2 = CO2 + H2O",
    "C3H8 + O2 = CO2 + H2O",
    "C4H10 + O2 = CO2 + H2O",
    "C13H28 + O2 = CO2 + H2O",
  ).map(s => s.ce)

  private def someEquations: Seq[ChemicalEquation] = Seq(
    "2 NaBr + F2 = 2 NaF + Br2",
    "2 K + 2 H2O = 2 KOH + H2",
    "2 H2O2 = 2 H2O + O2",
    "CuSO4 + 2 KCN = Cu(CN)2 + K2SO4",
    "P4 + 3 O2 = P4O6",
    "CH4 + 2 O2 = CO2 + 2 H2O",
    "N2 + 3 F2 = 2 NF3",
    "2 AlBr3 + 3 K2SO4 = 6 KBr + Al2(SO4)3",
    "2 K + Cl2 = 2 KCl",
    "2 Al (s) + 6 HCl (aq) = 3 H2 (g) + 2 AlCl3 (aq)",
    "N2 (g) + 3 H2 (g) = 2 NH3 (g)",
    "2 C2H6 + 7 O2 = 4 CO2 + 6 H2O",
    "C3H8 + 5 O2 = 3 CO2 + 4 H2O",
    "2 C4H10 + 13 O2 = 8 CO2 + 10 H2O",
    "C13H28 + 20 O2 = 13 CO2 + 14 H2O",
  ).map(s => s.ce.ensuring(ce => ce.isBalanced))

  private def someFormulas: Seq[Formula] = {
    ElementSymbol.diatomicElements.toSeq.map(e => ElementFormula(e, 2))
      .appendedAll(ElementSymbol.allElements.map(e => AtomicElement(e)))
      .appendedAll(Formula.WellKnownPolyatomicIons.values.toSeq)
      .appendedAll(someRawEquations.flatMap(_.reactantsAndProducts.map(_.formula)))
  }
}
