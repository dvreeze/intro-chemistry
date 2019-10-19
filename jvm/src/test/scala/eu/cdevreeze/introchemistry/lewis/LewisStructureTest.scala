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
  import LewisStructure._

  test("testElementN") {
    val lewisForN: LewisStructure = LewisStructure.builder.plusAtom(N.at(1), 5).build

    assertResult(true) {
      lewisForN.isConnectedGraph
    }

    assertResult(getValenceElectronCount(N)) {
      lewisForN.getAtom(N.at(1)).loneElectronCount
    }
  }

  test("testUnconnectedStructure") {
    val wrongLewis: LewisStructure = LewisStructure.builder.plusAtom(C.at(1), 4).plusAtom(H.at(1), 1).build

    assertResult(false) {
      wrongLewis.isConnectedGraph
    }
  }

  test("testElementCa") {
    val lewisForCa: LewisStructure = LewisStructure.builder.plusAtom(Ca.at(1), 2).build

    assertResult(true) {
      lewisForCa.isConnectedGraph
    }

    assertResult(getValenceElectronCount(Ca)) {
      lewisForCa.getAtom(Ca.at(1)).loneElectronCount
    }
  }

  test("testElementC") {
    val lewisForC: LewisStructure = LewisStructure.builder.plusAtom(C.at(1), 4).build

    assertResult(true) {
      lewisForC.isConnectedGraph
    }

    assertResult(getValenceElectronCount(C)) {
      lewisForC.getAtom(C.at(1)).loneElectronCount
    }
  }

  test("testWrongElementO") {
    intercept[RuntimeException] {
      LewisStructure.builder.plusAtom(O.at(1), 5).build
    }
  }

  test("testElementH") {
    val lewisForH: LewisStructure = LewisStructure.builder.plusAtom(H.at(1), 1).build

    assertResult(true) {
      lewisForH.isConnectedGraph
    }

    assertResult(getValenceElectronCount(H)) {
      lewisForH.getAtom(H.at(1)).loneElectronCount
    }
  }

  test("testElementNe") {
    val lewisForNe: LewisStructure = LewisStructure.builder.plusAtom(Ne.at(1), 8).build

    assertResult(true) {
      lewisForNe.isConnectedGraph
    }

    assertResult(getValenceElectronCount(Ne)) {
      lewisForNe.getAtom(Ne.at(1)).loneElectronCount
    }
  }

  test("testCO2") {
    val lewisForCO2: LewisStructure =
      LewisStructure.builder
        .plusAtom(C.at(1), 0).plusAtom(O.at(1), 4).plusAtom(O.at(2), 4)
        .plusBonds(C.at(1), O.at(1), 2)
        .plusBonds(C.at(1), O.at(2), 2)
        .build

    assertResult(true) {
      lewisForCO2.isConnectedGraph
    }

    assertResult(getValenceElectronCount(C)) {
      lewisForCO2.getElectronCount(C.at(1))
    }

    assertResult(8) {
      lewisForCO2.getSurroundingElectronCount(C.at(1))
    }

    assertResult(getValenceElectronCount(O)) {
      lewisForCO2.getElectronCount(O.at(1))
    }

    assertResult(8) {
      lewisForCO2.getSurroundingElectronCount(O.at(1))
    }

    assertResult(getValenceElectronCount(O)) {
      lewisForCO2.getElectronCount(O.at(2))
    }

    assertResult(8) {
      lewisForCO2.getSurroundingElectronCount(O.at(2))
    }

    assertResult("CO2".f.atomCounts) {
      lewisForCO2.atomCounts
    }
  }

  test("testN2") {
    val lewisForN2: LewisStructure =
      LewisStructure.builder
        .plusAtom(N.at(1), 2).plusAtom(N.at(2), 2)
        .plusBonds(N.at(1), N.at(2), 3)
        .build

    assertResult(true) {
      lewisForN2.isConnectedGraph
    }

    assertResult(getValenceElectronCount(N)) {
      lewisForN2.getElectronCount(N.at(1))
    }

    assertResult(8) {
      lewisForN2.getSurroundingElectronCount(N.at(1))
    }

    assertResult(getValenceElectronCount(N)) {
      lewisForN2.getElectronCount(N.at(2))
    }

    assertResult(8) {
      lewisForN2.getSurroundingElectronCount(N.at(2))
    }

    assertResult("N2".f.atomCounts) {
      lewisForN2.atomCounts
    }
  }

  test("testCH4") {
    val lewisForCH4: LewisStructure =
      LewisStructure.builder
        .plusAtom(C.at(1), 0).plusAtom(H.at(1), 0).plusAtom(H.at(2), 0).plusAtom(H.at(3), 0).plusAtom(H.at(4), 0)
        .plusBond(C.at(1), H.at(1))
        .plusBond(C.at(1), H.at(2))
        .plusBond(C.at(1), H.at(3))
        .plusBond(C.at(1), H.at(4))
        .build

    assertResult(true) {
      lewisForCH4.isConnectedGraph
    }

    assertResult(getValenceElectronCount(C)) {
      lewisForCH4.getElectronCount(C.at(1))
    }

    assertResult(8) {
      lewisForCH4.getSurroundingElectronCount(C.at(1))
    }

    assertResult(Set(getValenceElectronCount(H))) {
      1.to(4).map(i => lewisForCH4.getElectronCount(H.at(i))).toSet
    }

    assertResult(Set(2)) { // For H no octet rule
      1.to(4).map(i => lewisForCH4.getSurroundingElectronCount(H.at(i))).toSet
    }

    assertResult("CH4".f.atomCounts) {
      lewisForCH4.atomCounts
    }
  }

  test("testUnconnectedStructureForCl") {
    val wrongLewisForCl2: LewisStructure =
      LewisStructure.builder.plusAtom(Cl.at(1), 7).plusAtom(Cl.at(2), 7).build

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
        .plusAtom(N.at(1), 2).plusAtom(H.at(1), 0).plusAtom(H.at(2), 0).plusAtom(H.at(3), 0)
        .plusBond(N.at(1), H.at(1))
        .plusBond(N.at(1), H.at(2))
        .plusBond(N.at(1), H.at(3))
        .build

    assertResult(true) {
      lewisForNH3.isConnectedGraph
    }

    assertResult(true) {
      lewisForNH3.obeysOctetRule(N.at(1))
    }

    assertResult(false) {
      lewisForNH3.obeysOctetRule(H.at(1))
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
        .plusAtom(H.at(1), 0).plusAtom(C.at(1), 0).plusAtom(N.at(1), 2)
        .plusBond(H.at(1), C.at(1))
        .plusBonds(C.at(1), N.at(1), 3)
        .build

    validateIdealLewisStructure(lewisForHCN, "HCN".f)
  }

  test("testCreateLewisStructureForCF4") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(C.at(1), 4).plusAtom(F.at(1), 7).plusAtom(F.at(2), 7).plusAtom(F.at(3), 7).plusAtom(F.at(4), 7).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(C.at(1), F.at(1)).makeBond(C.at(1), F.at(2)).makeBond(C.at(1), F.at(3)).makeBond(C.at(1), F.at(4))

    validateIdealLewisStructure(lewis, "CF4".f)
  }

  test("testCreateLewisStructureForNF3") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(N.at(1), 5).plusAtom(F.at(1), 7).plusAtom(F.at(2), 7).plusAtom(F.at(3), 7).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(N.at(1), F.at(1)).makeBond(N.at(1), F.at(2)).makeBond(N.at(1), F.at(3))

    validateIdealLewisStructure(lewis, "NF3".f)

    assertResult(2) {
      lewis.getAtom(N.at(1)).loneElectronCount
    }
    assertResult(6) {
      lewis.getAtom(F.at(1)).loneElectronCount
    }
  }

  test("testCreateLewisStructureForH2O") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(H.at(1), 1).plusAtom(H.at(2), 1).plusAtom(O.at(1), 6).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(O.at(1), H.at(1)).makeBond(O.at(1), H.at(2))

    validateIdealLewisStructure(lewis, "H2O".f)

    assertResult(0) {
      lewis.getAtom(H.at(1)).loneElectronCount
    }
    assertResult(4) {
      lewis.getAtom(O.at(1)).loneElectronCount
    }
  }

  test("testCreateLewisStructureForN2H2") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(H.at(1), 1).plusAtom(H.at(2), 1).plusAtom(N.at(1), 5).plusAtom(N.at(2), 5).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(N.at(1), H.at(1)).makeBond(N.at(2), H.at(2)).makeDoubleBond(N.at(1), N.at(2))

    validateIdealLewisStructure(lewis, "N2H2".f)

    assertResult(0) {
      lewis.getAtom(H.at(1)).loneElectronCount
    }
    assertResult(2) {
      lewis.getAtom(N.at(1)).loneElectronCount
    }
  }

  test("testCreateLewisStructureForNO2Ion") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(N.at(1), 5).plusAtom(O.at(1), 6).plusAtom(O.at(2), 7).withCharge(-1).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeDoubleBond(N.at(1), O.at(1)).makeBond(N.at(1), O.at(2))

    validateLewisStructureForIon(lewis, "NO2{-1}".f)

    assertResult(2) {
      lewis.getAtom(N.at(1)).loneElectronCount
    }
    assertResult(4) {
      lewis.getAtom(O.at(1)).loneElectronCount
    }
    assertResult(6) {
      lewis.getAtom(O.at(2)).loneElectronCount
    }
  }

  test("testCreateLewisStructureForC2H2") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(H.at(1), 1).plusAtom(H.at(2), 1).plusAtom(C.at(1), 4).plusAtom(C.at(2), 4).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(C.at(1), H.at(1)).makeBond(C.at(2), H.at(2)).makeTripleBond(C.at(1), C.at(2))

    validateIdealLewisStructure(lewis, "C2H2".f)

    assertResult(0) {
      lewis.getAtom(H.at(1)).loneElectronCount
    }
    assertResult(0) {
      lewis.getAtom(C.at(1)).loneElectronCount
    }
  }

  test("testCreateLewisStructureForC2H4") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(H.at(1), 1).plusAtom(H.at(2), 1).plusAtom(H.at(3), 1).plusAtom(H.at(4), 1)
        .plusAtom(C.at(1), 4).plusAtom(C.at(2), 4).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(C.at(1), H.at(1)).makeBond(C.at(1), H.at(2)).makeBond(C.at(2), H.at(3)).makeBond(C.at(2), H.at(4))
        .makeDoubleBond(C.at(1), C.at(2))

    validateIdealLewisStructure(lewis, "C2H4".f)

    assertResult(0) {
      lewis.getAtom(H.at(1)).loneElectronCount
    }
    assertResult(0) {
      lewis.getAtom(C.at(1)).loneElectronCount
    }
  }

  test("testCreateLewisStructureForNH4Ion") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(N.at(1), 4)
        .plusAtom(H.at(1), 1).plusAtom(H.at(2), 1).plusAtom(H.at(3), 1).plusAtom(H.at(4), 1).withCharge(1).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(N.at(1), H.at(1)).makeBond(N.at(1), H.at(2)).makeBond(N.at(1), H.at(3)).makeBond(N.at(1), H.at(4))

    validateLewisStructureForIon(lewis, "NH4{1}".f)

    assertResult(0) {
      lewis.getAtom(N.at(1)).loneElectronCount
    }
    assertResult(0) {
      lewis.getAtom(H.at(1)).loneElectronCount
    }
    assertResult(0) {
      lewis.getAtom(H.at(4)).loneElectronCount
    }
  }

  /*
  test("testCreateLewisStructureForSOCl2") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(S.at(1), 6).plusAtom(O.at(1), 6).plusAtom(Cl.at(1), 7).plusAtom(Cl.at(2), 7).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeDoubleBond(S.at(1), O.at(1)).makeBond(S.at(1), Cl.at(1)).makeBond(S.at(1), Cl.at(2))

    validateIdealLewisStructure(lewis, "SOCl2".f)

    assertResult(0) {
      lewis.getAtom(S.at(1)).loneElectronCount
    }
    assertResult(4) {
      lewis.getAtom(O.at(1)).loneElectronCount
    }
    assertResult(7) {
      lewis.getAtom(Cl.at(1)).loneElectronCount
    }
    assertResult(7) {
      lewis.getAtom(Cl.at(2)).loneElectronCount
    }
  }
  */

  test("testCreateLewisStructureForCH2O") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(C.at(1), 4).plusAtom(H.at(1), 1).plusAtom(H.at(2), 1).plusAtom(O.at(1), 6).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeBond(C.at(1), H.at(1)).makeBond(C.at(1), H.at(2)).makeDoubleBond(C.at(1), O.at(1))

    validateIdealLewisStructure(lewis, "CH2O".f)

    assertResult(0) {
      lewis.getAtom(C.at(1)).loneElectronCount
    }
    assertResult(0) {
      lewis.getAtom(H.at(1)).loneElectronCount
    }
    assertResult(0) {
      lewis.getAtom(H.at(2)).loneElectronCount
    }
    assertResult(4) {
      lewis.getAtom(O.at(1)).loneElectronCount
    }
  }

  test("testCreateLewisStructureForCOCl2") {
    val startLewis: LewisStructure =
      LewisStructure.builder.plusAtom(C.at(1), 4).plusAtom(O.at(1), 6).plusAtom(Cl.at(1), 7).plusAtom(Cl.at(2), 7).build
        .ensuring(_.bonds.isEmpty)

    val lewis: LewisStructure =
      startLewis.makeDoubleBond(C.at(1), O.at(1)).makeBond(C.at(1), Cl.at(1)).makeBond(C.at(1), Cl.at(2))

    validateIdealLewisStructure(lewis, "COCl2".f)

    assertResult(0) {
      lewis.getAtom(C.at(1)).loneElectronCount
    }
    assertResult(4) {
      lewis.getAtom(O.at(1)).loneElectronCount
    }
    assertResult(6) {
      lewis.getAtom(Cl.at(1)).loneElectronCount
    }
    assertResult(6) {
      lewis.getAtom(Cl.at(2)).loneElectronCount
    }
  }

  test("testPossibleCyanateFormalCharges") {
    val possibleLewisStructures: Seq[LewisStructure] = Seq(
      LewisStructure.builder.plusAtom(C.at(1), 4).plusAtom(N.at(1), 7).plusAtom(O.at(1), 5).withCharge(-1).build
        .makeBond(C.at(1), N.at(1)).makeTripleBond(C.at(1), O.at(1)),
      LewisStructure.builder.plusAtom(C.at(1), 4).plusAtom(N.at(1), 5).plusAtom(O.at(1), 7).withCharge(-1).build
        .makeTripleBond(C.at(1), N.at(1)).makeBond(C.at(1), O.at(1)),
      LewisStructure.builder.plusAtom(C.at(1), 4).plusAtom(N.at(1), 6).plusAtom(O.at(1), 6).withCharge(-1).build
        .makeDoubleBond(C.at(1), N.at(1)).makeDoubleBond(C.at(1), O.at(1)),
      LewisStructure.builder.plusAtom(C.at(1), 6).plusAtom(N.at(1), 3).plusAtom(O.at(1), 7).withCharge(-1).build
        .makeDoubleBond(C.at(1), N.at(1)).makeBond(N.at(1), O.at(1)),
      LewisStructure.builder.plusAtom(C.at(1), 6).plusAtom(N.at(1), 4).plusAtom(O.at(1), 6).withCharge(-1).build
        .makeDoubleBond(C.at(1), N.at(1)).makeDoubleBond(N.at(1), O.at(1)),
    )

    possibleLewisStructures.foreach(lewis => validateLewisStructureForIon(lewis, "CNO{-1}".f))

    val expectedFormalChargeMappings: Seq[Map[AtomKey, Int]] = Seq(
      Map(C.at(1) -> 0, N.at(1) -> -2, O.at(1) -> 1),
      Map(C.at(1) -> 0, N.at(1) -> 0, O.at(1) -> -1), // close to ideal, and better than the next one (O is more electronegative than N)
      Map(C.at(1) -> 0, N.at(1) -> -1, O.at(1) -> 0), // close to ideal
      Map(C.at(1) -> -2, N.at(1) -> 2, O.at(1) -> -1),
      Map(C.at(1) -> -2, N.at(1) -> 1, O.at(1) -> 0),
    )

    assertResult(expectedFormalChargeMappings) {
      possibleLewisStructures.map(_.formalChargeMapping)
    }
  }

  /**
   * Validate the Lewis structure of a non-ion, expecting an ideal structure where all atom formal charges are 0 and all non-H atoms
   * obey the octet rule.
   */
  private def validateIdealLewisStructure(lewisStruct: LewisStructure, formula: Formula): Unit = {
    assert(lewisStruct.netCharge == 0)

    assertResult(true) {
      lewisStruct.isConnectedGraph
    }

    assertResult(formula.atomCounts) {
      lewisStruct.atomCounts
    }

    assertResult(getValenceElectronCount(formula)) {
      lewisStruct.electronCount
    }

    // The ideal of each atom in the Lewis structure having formal charge 0.
    assertResult(Set(0)) {
      lewisStruct.atomKeys.map(key => lewisStruct.getFormalCharge(key)).toSet
    }

    // The ideal of each non-hydrogen atom in the Lewis structure obeying the octet rule.
    assertResult(Set(8)) {
      lewisStruct.atoms.filterNot(_.key.element == H).map(atom => lewisStruct.getSurroundingElectronCount(atom.key)).toSet
    }
  }

  private def validateLewisStructureForIon(lewisStruct: LewisStructure, formula: Formula): Unit = {
    assert(lewisStruct.netCharge != 0)

    assertResult(true) {
      lewisStruct.isConnectedGraph
    }

    assertResult(formula.atomCounts) {
      lewisStruct.atomCounts
    }

    assertResult(formula.charge) {
      lewisStruct.netCharge
    }

    assertResult(getValenceElectronCount(formula)) {
      lewisStruct.electronCount
    }
  }
}
