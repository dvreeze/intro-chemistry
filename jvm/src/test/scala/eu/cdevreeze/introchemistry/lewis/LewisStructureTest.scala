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

package eu.cdevreeze.introchemistry.lewis

import eu.cdevreeze.introchemistry.api.SimpleQueryApi
import eu.cdevreeze.introchemistry.lewis.LewisStructure.AtomKey
import eu.cdevreeze.introchemistry.orbitals.AufbauPrinciple
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.stoichiometry.Formula
import org.scalatest.funsuite.AnyFunSuite

/**
 * Test of the LewisStructure, from the Chemistry course given by the University of Kentucky.
 *
 * @author Chris de Vreeze
 */
class LewisStructureTest extends AnyFunSuite {

  import AufbauPrinciple._
  import ElementSymbol._
  import SimpleQueryApi._

  test("testElementN") {
    val lewisForN: LewisStructure = LewisStructure.builder.plusAtom(N, 1, 5).build

    assertResult(true) {
      lewisForN.isConnectedGraph
    }

    assertResult(getValenceElectronCount(N)) {
      lewisForN.getAtom(AtomKey(N, 1)).loneElectronCount
    }
  }

  test("testUnconnectedStructure") {
    val wrongLewis: LewisStructure = LewisStructure.builder.plusAtom(C, 1, 4).plusAtom(H, 1, 1).build

    assertResult(false) {
      wrongLewis.isConnectedGraph
    }
  }

  test("testElementCa") {
    val lewisForCa: LewisStructure = LewisStructure.builder.plusAtom(Ca, 1, 2).build

    assertResult(true) {
      lewisForCa.isConnectedGraph
    }

    assertResult(getValenceElectronCount(Ca)) {
      lewisForCa.getAtom(AtomKey(Ca, 1)).loneElectronCount
    }
  }

  test("testElementC") {
    val lewisForC: LewisStructure = LewisStructure.builder.plusAtom(C, 1, 4).build

    assertResult(true) {
      lewisForC.isConnectedGraph
    }

    assertResult(getValenceElectronCount(C)) {
      lewisForC.getAtom(AtomKey(C, 1)).loneElectronCount
    }
  }

  test("testWrongElementO") {
    val lewisForO: LewisStructure = LewisStructure.builder.plusAtom(O, 1, 5).build // wrong

    assertResult(true) {
      lewisForO.isConnectedGraph
    }

    assertResult(false) {
      lewisForO.getAtom(AtomKey(O, 1)).loneElectronCount == getValenceElectronCount(O)
    }
  }

  test("testElementH") {
    val lewisForH: LewisStructure = LewisStructure.builder.plusAtom(H, 1, 1).build

    assertResult(true) {
      lewisForH.isConnectedGraph
    }

    assertResult(getValenceElectronCount(H)) {
      lewisForH.getAtom(AtomKey(H, 1)).loneElectronCount
    }
  }

  test("testElementNe") {
    val lewisForNe: LewisStructure = LewisStructure.builder.plusAtom(Ne, 1, 8).build

    assertResult(true) {
      lewisForNe.isConnectedGraph
    }

    assertResult(getValenceElectronCount(Ne)) {
      lewisForNe.getAtom(AtomKey(Ne, 1)).loneElectronCount
    }
  }

  test("testCO2") {
    val lewisForCO2: LewisStructure =
      LewisStructure.builder
        .plusAtom(C, 1, 0).plusAtom(O, 1, 4).plusAtom(O, 2, 4)
        .plusBonds(AtomKey(C, 1), AtomKey(O, 1), 2)
        .plusBonds(AtomKey(C, 1), AtomKey(O, 2), 2)
        .build

    assertResult(true) {
      lewisForCO2.isConnectedGraph
    }

    assertResult(getValenceElectronCount(C)) {
      lewisForCO2.getElectronCount(AtomKey(C, 1))
    }

    assertResult(8) {
      lewisForCO2.getSurroundingElectronCount(AtomKey(C, 1))
    }

    assertResult(getValenceElectronCount(O)) {
      lewisForCO2.getElectronCount(AtomKey(O, 1))
    }

    assertResult(8) {
      lewisForCO2.getSurroundingElectronCount(AtomKey(O, 1))
    }

    assertResult(getValenceElectronCount(O)) {
      lewisForCO2.getElectronCount(AtomKey(O, 2))
    }

    assertResult(8) {
      lewisForCO2.getSurroundingElectronCount(AtomKey(O, 2))
    }

    assertResult("CO2".f.atomCounts) {
      lewisForCO2.atomCounts
    }
  }

  test("testN2") {
    val lewisForN2: LewisStructure =
      LewisStructure.builder
        .plusAtom(N, 1, 2).plusAtom(N, 2, 2)
        .plusBonds(AtomKey(N, 1), AtomKey(N, 2), 3)
        .build

    assertResult(true) {
      lewisForN2.isConnectedGraph
    }

    assertResult(getValenceElectronCount(N)) {
      lewisForN2.getElectronCount(AtomKey(N, 1))
    }

    assertResult(8) {
      lewisForN2.getSurroundingElectronCount(AtomKey(N, 1))
    }

    assertResult(getValenceElectronCount(N)) {
      lewisForN2.getElectronCount(AtomKey(N, 2))
    }

    assertResult(8) {
      lewisForN2.getSurroundingElectronCount(AtomKey(N, 2))
    }

    assertResult("N2".f.atomCounts) {
      lewisForN2.atomCounts
    }
  }

  test("testCH4") {
    val lewisForCH4: LewisStructure =
      LewisStructure.builder
        .plusAtom(C, 1, 0).plusAtom(H, 1, 0).plusAtom(H, 2, 0).plusAtom(H, 3, 0).plusAtom(H, 4, 0)
        .plusBond(AtomKey(C, 1), AtomKey(H, 1))
        .plusBond(AtomKey(C, 1), AtomKey(H, 2))
        .plusBond(AtomKey(C, 1), AtomKey(H, 3))
        .plusBond(AtomKey(C, 1), AtomKey(H, 4))
        .build

    assertResult(true) {
      lewisForCH4.isConnectedGraph
    }

    assertResult(getValenceElectronCount(C)) {
      lewisForCH4.getElectronCount(AtomKey(C, 1))
    }

    assertResult(8) {
      lewisForCH4.getSurroundingElectronCount(AtomKey(C, 1))
    }

    assertResult(Set(getValenceElectronCount(H))) {
      1.to(4).map(i => lewisForCH4.getElectronCount(AtomKey(H, i))).toSet
    }

    assertResult(Set(2)) { // For H no octet rule
      1.to(4).map(i => lewisForCH4.getSurroundingElectronCount(AtomKey(H, i))).toSet
    }

    assertResult("CH4".f.atomCounts) {
      lewisForCH4.atomCounts
    }
  }

  test("testUnconnectedStructureForCl") {
    val wrongLewisForCl2: LewisStructure =
      LewisStructure.builder.plusAtom(Cl, 1, 7).plusAtom(Cl, 2, 7).build

    assertResult(false) {
      wrongLewisForCl2.isConnectedGraph
    }

    assertResult("Cl2".f.atomCounts) {
      wrongLewisForCl2.atomCounts
    }
  }

  test("testNH3") {
    val lewisForNH3: LewisStructure =
      LewisStructure.builder
        .plusAtom(N, 1, 2).plusAtom(H, 1, 0).plusAtom(H, 2, 0).plusAtom(H, 3, 0)
        .plusBond(AtomKey(N, 1), AtomKey(H, 1))
        .plusBond(AtomKey(N, 1), AtomKey(H, 2))
        .plusBond(AtomKey(N, 1), AtomKey(H, 3))
        .build

    assertResult(true) {
      lewisForNH3.isConnectedGraph
    }

    assertResult(true) {
      lewisForNH3.obeysOctetRule(AtomKey(N, 1))
    }

    assertResult(false) {
      lewisForNH3.obeysOctetRule(AtomKey(H, 1))
    }

    assertResult("NH3".f.atomCounts) {
      lewisForNH3.atomCounts
    }

    assertResult(lewisForNH3.atoms.map(atom => getValenceElectronCount(atom.key.element))) {
      lewisForNH3.atoms.map(atom => lewisForNH3.getElectronCount(atom.key))
    }

    assertResult(2) {
      lewisForNH3.atoms.map(_.loneElectronCount).sum
    }
  }

  test("testHCN") {
    val lewisForHCN: LewisStructure =
      LewisStructure.builder
        .plusAtom(H, 1, 0).plusAtom(C, 1, 0).plusAtom(N, 1, 2)
        .plusBond(AtomKey(H, 1), AtomKey(C, 1))
        .plusBonds(AtomKey(C, 1), AtomKey(N, 1), 3)
        .build

    validateLewisStructure(lewisForHCN, "HCN".f)
  }

  private def validateLewisStructure(lewisStruct: LewisStructure, formula: Formula): Unit = {
    assertResult(true) {
      lewisStruct.isConnectedGraph
    }

    assertResult(formula.atomCounts) {
      lewisStruct.atomCounts
    }

    assertResult(getValenceElectronCount(formula)) {
      lewisStruct.electronCount
    }

    assertResult(lewisStruct.atoms.map(atom => getValenceElectronCount(atom.key.element))) {
      lewisStruct.atoms.map(atom => lewisStruct.getElectronCount(atom.key))
    }

    assertResult(Set(8)) {
      lewisStruct.atoms.filterNot(_.key.element == H).map(atom => lewisStruct.getSurroundingElectronCount(atom.key)).toSet
    }
  }
}
