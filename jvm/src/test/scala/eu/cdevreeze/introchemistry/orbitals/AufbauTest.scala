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

package eu.cdevreeze.introchemistry.orbitals

import org.scalatest.funsuite.AnyFunSuite

import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * Test of the Aufbau principle in action.
 *
 * @author Chris de Vreeze
 */
class AufbauTest extends AnyFunSuite {

  import ElementSymbol._

  test("testElectronConfigOfH") {
    assertResult(ElectronConfig.parse("1s1")) {
      AufbauPrinciple.getProbableAbsoluteElectronConfig(H)
    }

    assertResult(ElectronConfig.parse("1s1")) {
      AufbauPrinciple.getProbableElectronConfig(H)
    }
  }

  test("testElectronConfigOfC") {
    assertResult(ElectronConfig.parse("1s2 2s2 2p2")) {
      AufbauPrinciple.getProbableAbsoluteElectronConfig(C)
    }

    assertResult(ElectronConfig.parse("[He]2s2 2p2")) {
      AufbauPrinciple.getProbableElectronConfig(C)
    }
  }

  test("testElectronConfigOfO") {
    assertResult(ElectronConfig.parse("1s2 2s2 2p4")) {
      AufbauPrinciple.getProbableAbsoluteElectronConfig(O)
    }

    assertResult(ElectronConfig.parse("[He]2s2 2p4")) {
      AufbauPrinciple.getProbableElectronConfig(O)
    }
  }

  test("testElectronConfigOfSi") {
    assertResult(ElectronConfig.parse("1s2 2s2 2p6 3s2 3p2")) {
      AufbauPrinciple.getProbableAbsoluteElectronConfig(Si)
    }

    assertResult(ElectronConfig.parse("[Ne]3s2 3p2")) {
      AufbauPrinciple.getProbableElectronConfig(Si)
    }
  }

  test("testElectronConfigOfSb") { // Antimony
    assertResult(ElectronConfig.parse("1s2 2s2 2p6 3s2 3p6 4s2 3d10 4p6 5s2 4d10 5p3")) {
      AufbauPrinciple.getProbableAbsoluteElectronConfig(Sb)
    }

    assertResult(ElectronConfig.parse("[Kr]5s2 4d10 5p3")) {
      AufbauPrinciple.getProbableElectronConfig(Sb)
    }
  }

  test("testElectronConfigOfCr") {
    assertResult(ElectronConfig.parse("[Ar]4s1 3d5")) { // exception!
      AufbauPrinciple.getProbableElectronConfig(Cr)
    }
  }

  test("testElectronConfigOfAg") {
    assertResult(ElectronConfig.parse("[Kr]5s1 4d10")) { // exception!
      AufbauPrinciple.getProbableElectronConfig(Ag)
    }
  }

  test("testElectronConfigOfOg") {
    assertResult(ElectronConfig.parse("[Rn]7s2 5f14 6d10 7p6")) {
      AufbauPrinciple.getProbableElectronConfig(Og)
    }
  }

  test("testElectronConfigOfNiIon") {
    assertResult(ElectronConfig.parse("[Ar]4s2 3d8")) {
      AufbauPrinciple.getProbableElectronConfig(Ni)
    }

    assertResult(ElectronConfig.parse("[Ar]3d8")) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Ni, +2)
    }
  }

  test("testElectronConfigOfSnIon") {
    assertResult(ElectronConfig.parse("[Kr]5s2 4d10 5p2")) {
      AufbauPrinciple.getProbableElectronConfig(Sn)
    }

    assertResult(ElectronConfig.parse("[Kr]5s1 4d10")) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Sn, +3)
    }
  }

  test("testElectronConfigOfFeIon") {
    assertResult(ElectronConfig.parse("[Ar]4s2 3d6")) {
      AufbauPrinciple.getProbableElectronConfig(Fe)
    }

    assertResult(ElectronConfig.parse("[Ar]3d5")) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Fe, +3)
    }
  }

  test("testIsoElectronicFAndOIons") {
    assertResult(AufbauPrinciple.getProbableElectronConfig(Ne)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(F, -1)
    }

    assertResult(AufbauPrinciple.getProbableElectronConfigOfIon(F, -1)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(O, -2)
    }

    assertResult(AufbauPrinciple.getProbableElectronConfigOfIon(F, -1)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Na, +1)
    }

    assertResult(AufbauPrinciple.getProbableElectronConfigOfIon(F, -1)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Mg, +2)
    }
  }

  test("testNotIsoElectronic") {
    assertResult(V.atomicNumber) {
      AufbauPrinciple.getElectronCount(V)
    }

    assertResult(V.atomicNumber) {
      AufbauPrinciple.getElectronCount(AufbauPrinciple.getProbableElectronConfigOfIon(Fe, +3))
    }

    assertResult(ElectronConfig.parse("[Ar]3d5")) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Fe, +3)
    }

    assertResult(false) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Fe, +3) == AufbauPrinciple.getProbableElectronConfig(V)
    }
  }

  test("testIonsIsoElectronicWithAr") {
    assertResult(ElectronConfig.parse("[Ne]3s2 3p6")) {
      AufbauPrinciple.getProbableElectronConfig(Ar)
    }

    assertResult(AufbauPrinciple.getProbableElectronConfig(Ar)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Ca, +2)
    }

    assertResult(AufbauPrinciple.getProbableElectronConfig(Ar)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(K, +1)
    }

    assertResult(AufbauPrinciple.getProbableElectronConfig(Ar)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(Cl, -1)
    }

    assertResult(AufbauPrinciple.getProbableElectronConfig(Ar)) {
      AufbauPrinciple.getProbableElectronConfigOfIon(S, -2)
    }
  }

  test("testProtonAndElectronCountsMatch") {
    import AufbauPrinciple._

    val elementAtomicNumbers: Map[ElementSymbol, Int] = ElementSymbol.allElements.map(elem => elem -> elem.atomicNumber).toMap

    assertResult(elementAtomicNumbers) {
      ElementSymbol.allElements.map(elem => elem -> getElectronCount(elem)).toMap
    }
  }
}
