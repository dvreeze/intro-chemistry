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

package eu.cdevreeze.introchemistry.summarizelecture.covalentbonding

import eu.cdevreeze.introchemistry.api.SimpleQueryApi
import eu.cdevreeze.introchemistry.orbitals.AufbauPrinciple
import eu.cdevreeze.introchemistry.lewis._
import eu.cdevreeze.introchemistry.periodictable.ElementSymbol
import eu.cdevreeze.introchemistry.stoichiometry.Formula
import org.scalatest.funsuite.AnyFunSuite

/**
 * The lecture on covalent bonding summarized, from the Chemistry course given by the University of Kentucky.
 *
 * @author Chris de Vreeze
 */
class SummarizeCovalentBondingTest extends AnyFunSuite {

  import ElementSymbol._
  import SimpleQueryApi._
  import AufbauPrinciple._
  import LewisStructure._

  // For VSEPR theory ("vesper"), also see
  // https://chem.libretexts.org/Bookshelves/General_Chemistry/Map%3A_Chemistry_-_The_Central_Science_(Brown_et_al.)/09._Molecular_Geometry_and_Bonding_Theories/9.2%3A_The_VSEPR_Model

  test("testDetermineGeometryOfNF3") {
    // First note that NF3 must be a molecule, since it only contains non-metals.

    // Determine the electron count.

    val formula = "NF3".f
    val electronCount = getValenceElectronCount(formula)

    assertResult(Map(N -> 5, F -> 7)) {
      formula.elementSymbols.map(e => e -> getValenceElectronCount(e)).toMap
    }
    assertResult(26) {
      electronCount
    }

    // Now try to create a Lewis structure. First step: the core structure. The least electronegative atom is at the center.

    val lewisBuilder1: LewisStructure.Builder =
      LewisStructure.builder.plusRawAtom(N.at(1)).plusRawAtom(F.at(1)).plusRawAtom(F.at(2)).plusRawAtom(F.at(3))
        .plusBond(N.at(1), F.at(1)).plusBond(N.at(1), F.at(2)).plusBond(N.at(1), F.at(3))

    assertResult(6) {
      lewisBuilder1.electronCount
    }

    // Next step: fill outer atoms with lone electron pairs (from the remaining electrons).

    val lewisBuilder2: LewisStructure.Builder =
      lewisBuilder1.incrementLoneElectronCount(F.at(1), 6).incrementLoneElectronCount(F.at(2), 6).incrementLoneElectronCount(F.at(3), 6)

    assertResult(24) {
      lewisBuilder2.electronCount
    }

    // Final step of building the Lewis structure: fill the core atom with the remaining electrons.

    val lewisStruct: LewisStructure = lewisBuilder2.incrementLoneElectronCount(N.at(1), 2).build

    validateLewisStructure(lewisStruct, formula)

    // Determine the bonding groups and lone pair groups (of the central atom), which summed up determine the electron groups.

    val bondingGroupCount: Int = lewisStruct.findBonds(N.at(1)).map(b => b.to).distinct.size
    val lonePairGroups: Int = lewisStruct.getAtom(N.at(1)).loneElectronCount / 2

    assertResult(3) {
      bondingGroupCount
    }
    assertResult(1) {
      lonePairGroups
    }

    // The molecule is therefore designated as AX3E. Here A is the central atom, X is a bonding group, and E is a nonbonding group (lone electron pair).

    // This translates to: the number of electron groups determines the electron group geometry, which is here tetrahedral.
    // The AXmEn notation implies the following molecular geometry: trigonal pyramidal.
  }

  test("testDetermineGeometryOfBH3") {
    // First note that BH3 must be a molecule, since it only contains non-metals.

    // Determine the electron count.

    val formula = "BH3".f
    val electronCount = getValenceElectronCount(formula)

    assertResult(Map(B -> 3, H -> 1)) {
      formula.elementSymbols.map(e => e -> getValenceElectronCount(e)).toMap
    }
    assertResult(6) {
      electronCount
    }

    // Now try to create a Lewis structure. First step: the core structure. The least electronegative atom is at the center.

    val lewisBuilder1: LewisStructure.Builder =
      LewisStructure.builder.plusRawAtom(B.at(1)).plusRawAtom(H.at(1)).plusRawAtom(H.at(2)).plusRawAtom(H.at(3))
        .plusBond(B.at(1), H.at(1)).plusBond(B.at(1), H.at(2)).plusBond(B.at(1), H.at(3))

    assertResult(6) {
      lewisBuilder1.electronCount
    }

    // Next step: fill outer atoms with lone electron pairs (from the remaining electrons).

    val lewisBuilder2: LewisStructure.Builder = lewisBuilder1

    assertResult(6) {
      lewisBuilder2.electronCount
    }

    // Final step of building the Lewis structure: fill the core atom with the remaining electrons.

    val lewisStruct: LewisStructure = lewisBuilder2.build

    validateLewisStructure(lewisStruct, formula)

    // Determine the bonding groups and lone pair groups (of the central atom), which summed up determine the electron groups.

    val bondingGroupCount: Int = lewisStruct.findBonds(B.at(1)).map(b => b.to).distinct.size
    val lonePairGroups: Int = lewisStruct.getAtom(B.at(1)).loneElectronCount / 2

    assertResult(3) {
      bondingGroupCount
    }
    assertResult(0) {
      lonePairGroups
    }

    // The molecule is therefore designated as AX3. Here A is the central atom, X is a bonding group, and E is a nonbonding group (lone electron pair).

    // This translates to: the number of electron groups determines the electron group geometry, which is here trigonal planar.
    // The AXmEn notation implies the following molecular geometry: trigonal planar.
  }

  test("testDetermineGeometryOfNitrite") {
    // First note that nitrite (NO2{-1}) must be a polyatomic ion, containing no metals.

    // Determine the electron count.

    val formula = "NO2{-1}".f
    val electronCount = getValenceElectronCount(formula)

    assertResult(Map(N -> 5, O -> 6)) {
      formula.elementSymbols.map(e => e -> getValenceElectronCount(e)).toMap
    }
    assertResult(18) {
      electronCount
    }

    // Now try to create a Lewis structure. First step: the core structure. The least electronegative atom is at the center.

    val lewisBuilder1: LewisStructure.Builder =
      LewisStructure.builder.plusRawAtom(N.at(1)).plusRawAtom(O.at(1)).plusRawAtom(O.at(2))
        .plusBonds(N.at(1), O.at(1), 2).plusBond(N.at(1), O.at(2))
        .withCharge(-1)

    assertResult(6) {
      lewisBuilder1.electronCount
    }

    // Next step: fill outer atoms with lone electron pairs (from the remaining electrons).

    val lewisBuilder2: LewisStructure.Builder =
      lewisBuilder1.incrementLoneElectronCount(O.at(1), 4).incrementLoneElectronCount(O.at(2), 6)

    assertResult(16) {
      lewisBuilder2.electronCount
    }

    // Final step of building the Lewis structure: fill the core atom with the remaining electrons.

    val lewisStruct: LewisStructure = lewisBuilder2.incrementLoneElectronCount(N.at(1), 2).build

    validateLewisStructureForIon(lewisStruct, formula)

    // Determine the bonding groups and lone pair groups (of the central atom), which summed up determine the electron groups.

    val bondingGroupCount: Int = lewisStruct.findBonds(N.at(1)).map(b => b.to).distinct.size
    val lonePairGroups: Int = lewisStruct.getAtom(N.at(1)).loneElectronCount / 2

    assertResult(2) {
      bondingGroupCount
    }
    assertResult(1) {
      lonePairGroups
    }

    // The molecule is therefore designated as AX2E. Here A is the central atom, X is a bonding group, and E is a nonbonding group (lone electron pair).

    // This translates to: the number of electron groups determines the electron group geometry, which is here trigonal planar.
    // The AXmEn notation implies the following molecular geometry: bent (V-shaped).
  }

  /**
   * Validate the Lewis structure of a non-ion, but not necessarily expecting an ideal structure where all atom formal charges would be 0 and
   * where all non-H atoms would obey the octet rule.
   */
  private def validateLewisStructure(lewisStruct: LewisStructure, formula: Formula): Unit = {
    assert(lewisStruct.netCharge == 0)

    assertResult(true) {
      lewisStruct.underlyingGraphIsTree
    }

    assertResult(formula.atomCounts) {
      lewisStruct.atomCounts
    }

    assertResult(getValenceElectronCount(formula)) {
      lewisStruct.electronCount
    }
  }

  private def validateLewisStructureForIon(lewisStruct: LewisStructure, formula: Formula): Unit = {
    assert(lewisStruct.netCharge != 0)

    assertResult(true) {
      lewisStruct.underlyingGraphIsTree
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
