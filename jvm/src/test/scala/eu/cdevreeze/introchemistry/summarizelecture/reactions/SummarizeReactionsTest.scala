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

package eu.cdevreeze.introchemistry.summarizelecture.reactions

import eu.cdevreeze.introchemistry.api.SimpleQueryApi
import eu.cdevreeze.introchemistry.periodictable.load.PeriodicTableLoader
import eu.cdevreeze.introchemistry.stoichiometry.ChemicalEquation
import org.scalatest.funsuite.AnyFunSuite

/**
 * The lecture on reactions summarized, from the Chemistry course given by the University of Kentucky.
 *
 * @author Chris de Vreeze
 */
class SummarizeReactionsTest extends AnyFunSuite {

  private val queryApi = new SimpleQueryApi(PeriodicTableLoader.newInstance().loadPeriodicTable())

  import queryApi._

  test("writingBalancedChemicalEquations") {
    // Reaction of sodium bicarbonate ("baking soda") and acetic acid ("vinegar"), yielding hydrogen oxide (water) and
    // carbon dioxide.

    val ce1 = "1 NaHCO3 + 1 HC2H3O2 --> 1 NaC2H3O2 + 1 H2O + 1 CO2".ce

    assertResult(true) {
      ce1.isBalanced
    }

    // Combustion reaction of methane. Combustion uses oxygen, and yields carbon dioxide and water.

    val rawCe2 = "1 CH4 (g) + 1 O2 (g) --> 1 CO2 (g) + 1 H2O (l)".ce
    val ce2 = tryToBalanceChemicalEquation(rawCe2).get

    assertResult("1 CH4 (g) + 2 O2 (g) --> 1 CO2 (g) + 2 H2O (l)".ce) {
      ce2
    }

    // Reaction of silicon tetrachloride and water, yielding silicon dioxide and hydrochloric acid.

    val rawCe3 = "1 SiCl4 + 1 H2O --> 1 SiO2 + 1 HCl".ce
    val ce3 = tryToBalanceChemicalEquation(rawCe3).get

    assertResult("1 SiCl4 + 2 H2O --> 1 SiO2 + 4 HCl".ce) {
      ce3
    }

    // Reaction of iron and oxygen.

    val rawCe4 = "1 Fe + 1 O2 --> 1 Fe2O3".ce
    val ce4 = tryToBalanceChemicalEquation(rawCe4).get

    assertResult("4 Fe + 3 O2 --> 2 Fe2O3".ce) {
      ce4
    }
  }

  test("writingBalancedChemicalEquationsExercise") {
    val rawEquations: Seq[ChemicalEquation] = Seq(
      "1 NaBr + 1 F2 --> 1 NaF + 1 Br2",
      "1 K + 1 H2O --> 1 KOH + 1 H2",
      "1 H2O2 --> 1 H2O + 1 O2",
      "1 CuSO4 + 1 KCN --> 1 Cu(CN)2 + 1 K2SO4",
      "1 P4 + 1 O2 --> 1 P4O6",
      "1 CH4 + 1 O2 --> 1 CO2 + 1 H2O",
      "1 N2 + 1 F2 --> 1 NF3",
      "1 AlBr3 + 1 K2SO4 --> 1 KBr + 1 Al2(SO4)3",
      "1 K + 1 Cl2 --> 1 KCl",
      "1 Al (s) + 1 HCl (aq) --> 1 H2 (g) + 1 AlCl3 (aq)",
      "1 N2 (g) + 1 H2 (g) --> 1 NH3 (g)",
      "1 C2H6 + 1 O2 --> 1 CO2 + 1 H2O",
      "1 C3H8 + 1 O2 --> 1 CO2 + 1 H2O",
      "1 C4H10 + 1 O2 --> 1 CO2 + 1 H2O",
      "1 C13H28 + 1 O2 --> 1 CO2 + 1 H2O",
    ).map(s => s.ce)

    val expectedEquations: Seq[ChemicalEquation] = Seq(
      "2 NaBr + 1 F2 --> 2 NaF + 1 Br2",
      "2 K + 2 H2O --> 2 KOH + 1 H2",
      "2 H2O2 --> 2 H2O + 1 O2",
      "1 CuSO4 + 2 KCN --> 1 Cu(CN)2 + 1 K2SO4",
      "1 P4 + 3 O2 --> 1 P4O6",
      "1 CH4 + 2 O2 --> 1 CO2 + 2 H2O",
      "1 N2 + 3 F2 --> 2 NF3",
      "2 AlBr3 + 3 K2SO4 --> 6 KBr + 1 Al2(SO4)3",
      "2 K + 1 Cl2 --> 2 KCl",
      "2 Al (s) + 6 HCl (aq) --> 3 H2 (g) + 2 AlCl3 (aq)",
      "1 N2 (g) + 3 H2 (g) --> 2 NH3 (g)",
      "2 C2H6 + 7 O2 --> 4 CO2 + 6 H2O",
      "1 C3H8 + 5 O2 --> 3 CO2 + 4 H2O",
      "2 C4H10 + 13 O2 --> 8 CO2 + 10 H2O",
      "1 C13H28 + 20 O2 --> 13 CO2 + 14 H2O",
    ).map(s => s.ce.ensuring(ce => ce.isBalanced))

    assertResult(expectedEquations) {
      rawEquations.map(ce => tryToBalanceChemicalEquation(ce).get)
    }
  }

  test("aqueousSolutions") {
    // Solutions can be anything, in any phase (!), where the solvent is the substance of which we have the most and the solute
    // is the other substance. In aqueous solutions, the solvent is water, and a typical solute could be table salt.

    // Electrolytes (in water) are substances that produce an electrically conducting solution when dissolved in water.
    // The dissolved electrolyte separates into cations and anions. An negative electrode put into the solution would attract
    // the cations, and a positive electrode would attract the anions (opposites attract).

    // Strong electrolytes are substances that completely dissolve in water (or in the molten state) and that are capable of conducting electricity.
    // Weak electrolytes are substances that partially dissolve in water and that are therefore poor conductors of electricity.
    // Non-electrolytes do not dissolve in water and do not conduct electricity.

    // Strong electrolyte: table salt

    assertResult(true) {
      "1 NaCl (s) --> 1 ion(Na, +1) (aq) + 1 ion(Cl, -1) (aq)".ce.isBalanced
    }

    // Weak electrolyte: acetic acid

    assertResult(true) {
      "1 CH3COOH (l) + 1 H2O (l) --> 1 ion(CH3COO, -1) (aq) + 1 ion(H3O, +1) (aq)".ce.isBalanced
    }

    // Non-electrolyte: simple sugar

    assertResult(true) {
      "1 C6H12O6 (s) --> 1 C6H12O6 (aq)".ce.isBalanced
    }

    // Strong electrolytes are ionic compounds, and for example HCl, HNO3, HClO4 and NaOH.
    // Put differently, strong acids, strong bases and ionic compounds typically make strong electrolytes.

    // Weak electrolytes are for example CH3COOH, HF, HNO2 and H2O.
    // Put differently, weak acids and bases typically make weak electrolytes.

    // Non-electrolytes are for example (NH2)2CO, CH3OH, C2H5OH and C12H22O11.
    // Hydrocarbons and large organic molecules are typically non-electrolytes.

    // Strong acids are for example HCl, HBr, HI, HNO3, H2SO4 and HClO4. Note that (these) acids acts as proton donor during ionization.
    // Note that HNO3 is a strong acid but HNO2 is a weak acid. Also, H2SO4 is a strong acid, but HSO4 is a weak acid.

    // Strong bases are for example LiOH, NaOH, KOH, Ca(OH)2, Sr(OH)2 and Ba(OH)2. Note that (these) bases contain hydroxide
    // ions and act as proton acceptors during ionization.

    // Note that weak acids or bases can still be dangerous.
  }

  test("solubilityOfIonicCompounds") {
    // Solubility is the maximum amount of solute that will dissolve in a given quantity of the solvent at a given temperature.

    // Recall that all ionic compounds are electrolytes, yet not all electrolytes are soluble in water.

    // Below are the solubility rules (in water), from
    // https://chem.libretexts.org/Bookshelves/Physical_and_Theoretical_Chemistry_Textbook_Maps/Supplemental_Modules_(Physical_and_Theoretical_Chemistry)/Equilibria/Solubilty/Solubility_Rules

    // 1. Salts containing group 1 elements (ion(Li, +1), ion(Na, +1), ion(K, +1) etc.) are soluble. So are salts containing the ammonium ion.
    // 2. Salts containing the nitrate ion are generally soluble.
    // 3. Salts containing the chloride, bromide and iodide anions are generally soluble. Exceptions are halide salts of silver, lead and mercury.
    // 4. Most silver salts are insoluble, but exceptions are AgNO3 and Ag(C2H3O2).
    // 5. Most sulfate salts are soluble, but exceptions include CaSO4, BaSO4, PbSO4, Ag2SO4 and SrSO4.
    // 6. Most hydroxide salts are only slightly soluble. Hydroxide salts of group 1 elements are soluble, and those of group 2 elements are slightly soluble.
    //    Those of transition metals and ion(Al, +3) are insoluble.
    // 7. Most sulfides (that is, sulfides, not sulfites) of transition metals are insoluble, and so are sulfides of arsenic, antimony, bismuth and lead.
    //    According to the chemistry course from the University of Kentucky, exceptions are sulfides of calcium, strontium (Sr) and barium, that are all soluble in water to a limited extent.
    // 8. Carbonates are generally insoluble, in particular group 2 carbonates and FeCO3 and PbCO3.
    // 9. Chromates are generally insoluble, such as PbCrO4 and BaCrO4.
    // 9. Phosphates are frequently insoluble.
    // 10. Fluorides are frequently insoluble.

    // If 2 rules above contradict each other, the first one applies. Another rule (in the course) says that most acetates are soluble.

    val someSolubleIonicCompounds = Set("NaBr".f, "Mg(NO3)2".f, "K2SO4".f, "Sr(OH)2".f, "NH4Cl".f, "AgNO3".f, "NaC2H3O2".f, "LiCl".f, "NaOH".f, "MgSO4".f, "Sr(NO3)2".f)

    val someInsolubleIonicCompounds = Set("MgCO3".f, "Al2S3".f, "CaSO4".f, "PbBr2".f, "Mg3(PO4)2".f, "BaSO4".f, "K3PO4".f, "CaCO3".f)

    assertResult(true) { // Tests nothing, of course.
      someSolubleIonicCompounds.intersect(someInsolubleIonicCompounds).isEmpty
    }
  }

  test("precipitationReactions") {
    // Reactions can result in precipitates. The precipitate is the insoluble solid that separates from the solution.
    // Use the solubility rules for predicting the precipitate.

    // Copper(II) sulfide precipitate

    assertResult(true) {
      "1 (NH4)2S (aq) + 1 Cu(NO3)2 (aq) --> 2 (NH4)(NO3) (aq) + 1 CuS (s)".ce.isBalanced
    }

    // No reaction, no precipitation

    assertResult(true) {
      "1 Na2SO4 (aq) + 1 MgCl2 (aq) --> 1 MgSO4 (aq) + 2 NaCl (aq)".ce.isBalanced
    }
  }

  test("molecularAndIonicAndNetIonicEquations") {
    // Consider precipitation reactions again. We can start with a molecular equation, turn it into a complete ionic equation,
    // and finally turn that one into a net ionic equation. The molecular equation does not show what is actually happenning.

    // Precipitation of copper(II) sulfide

    val molecularCe = "1 (NH4)2S (aq) + 1 Cu(NO3)2 (aq) --> 2 NH4NO3 (aq) + 1 CuS (s)".ce
    assertResult(true)(molecularCe.isBalanced)

    // Write the aqueous substances as ions, in an ionic equation
    val ionicCe =
      "2 ion(NH4, 1) (aq) + 1 ion(S, -2) (aq) + 1 ion(Cu, 2) (aq) + 2 ion(NO3, -1) (aq) --> 2 ion(NH4, 1) (aq) + 2 ion(NO3, -1) (aq) + 1 CuS (s)".ce // with spectator ions
    assertResult(true)(ionicCe.isBalanced)

    val netIonicCe = ionicCe.withoutDuplicates

    assertResult("1 ion(S, -2) (aq) + 1 ion(Cu, 2) (aq) --> 1 CuS (s)".ce) {
      netIonicCe
    }
    assertResult(true)(netIonicCe.isBalanced)

    // Precipitation of copper(II) carbonate

    val molecularCe1 = "1 K2CO3 (aq) + 1 Cu(NO3)2 (aq) --> 2 KNO3 (aq) + 1 CuCO3 (s)".ce
    assertResult(true)(molecularCe1.isBalanced)

    // Write the aqueous substances as ions, in an ionic equation
    val ionicCe1 =
      "2 ion(K, 1) (aq) + 1 ion(CO3, -2) (aq) + 1 ion(Cu, 2) (aq) + 2 ion(NO3, -1) (aq) --> 2 ion(K, 1) (aq) + 2 ion(NO3, -1) (aq) + 1 CuCO3 (s)".ce // with spectator ions
    assertResult(true)(ionicCe1.isBalanced)

    val netIonicCe1 = ionicCe1.withoutDuplicates

    assertResult("1 ion(CO3, -2) (aq) + 1 ion(Cu, 2) (aq) --> 1 CuCO3 (s)".ce) {
      netIonicCe1
    }
    assertResult(true)(netIonicCe1.isBalanced)
  }

  test("acidBaseReactions") {
    val ce = "1 CaCO3 (s) + 1 H2SO4 (aq) --> 1 CaSO4 (s) + 1 H2O (l) + 1 CO2 (g)".ce
    assertResult(true)(ce.isBalanced)

    // Acids typically have sour taste, cause color changes in plant dyes, react with some metals to produce hydrogen gas,
    // react with carbonates and bicarbonates to produce CO2 gas, and aqueous acid solutions conduct electricity.

    // Bases typically have bitter taste, are slippery (e.g. soaps), cause color changes in plant dyes, and aqueous base solutions conduct electricity.

    // Arrhenius acid: substance that produces ion(H, 1) (hydronium: ion(H3O, 1)) in water.
    // Arrhenius base: substance that produces hydroxide anion in water.

    val arrheniusAcidCe = "1 HCl + 1 H2O --> 1 ion(H3O, +1) + 1 ion(Cl, -1)".ce
    assertResult(true)(arrheniusAcidCe.isBalanced)

    val arrheniusBaseCe = "1 NH3 + 1 H2O --> 1 ion(NH4, +1) + 1 ion(OH, -1)".ce
    assertResult(true)(arrheniusBaseCe.isBalanced)

    // Bronsted acid: proton donor. More generic than Arrhenius acid.
    // Bronsted base: proton acceptor. More generic than Arrhenius base.
    // Typical "Bronsted acid/base reactions" both turn a base into an acid, and an acid into a base.

    // Acid-base neutralization reaction: between acid and base, producing salt and water. Salts are all strong electrolytes.
    // At the equivalence point of the acid-base reaction the moles of ion(H, 1) and the moles of ion(OH, -1) are equal.

    val ce1 = "1 HCl (aq) + 1 NaOH (aq) --> 1 NaCl (aq) + 1 H2O (l)".ce
    assertResult(true)(ce1.isBalanced)

    val ce2 = "1 HBr (aq) + 1 KOH (aq) --> 1 KBr (aq) + 1 H2O (l)".ce
    assertResult(true)(ce2.isBalanced)

    val ce2NetIonic = "1 ion(H, +1) (aq) + 1 ion(OH, -1) (aq) --> 1 H2O (l)".ce // The same for all such reactions
    assertResult(true)(ce2NetIonic.isBalanced)

    val ce3 = "2 HNO3 (aq) + 1 Ba(OH)2 (aq) --> 1 Ba(NO3)2 (aq) + 2 H2O (l)".ce
    assertResult(true)(ce3.isBalanced)
  }

  test("redoxReactions") {
    // Oxidation-reduction reactions, or redox reactions, are electron transfer reactions. Atoms change oxidation number.
    // Compare them with acid-base reactions where instead of electrons protons are transferred.

    // Redox reactions can be written as a pair of half-reactions, each of them either losing or gaining electrons.
    // Oxidation is the loss of electrons, and reduction is the gain of electrons. Reduction indeed means: reduction of charge.

    // Free elements have oxidation number 0. For example: Li, Ni, O2, Cl2.
    // Monatomic ions have the ionic charge as oxidation number. For example: ion(Li, 1), ion(F, -1), ion(Ti, 4).
    // Oxygen has oxidation number -2. For example: MgO, TiO2. It has oxidation number -1 only in H2O2 and ion(O2, -2).
    // Hydrogen has oxidation number 1. For example: H2O, HCl. It has oxidation number -1 in binary metal compounds. For example: LiH, CaH2.
    // Fluorine has oxidation number -1. For example: HF, MgF2.
    // Other halides typically have oxidation number -1. For example: HCl, NaBr. Examples of positive values: KClO3, HIO4.

    // Molecules must be neutral, so the sum of the oxidation states of the atoms must be zero.
    // For polyatomic ions the sum of the oxidation states must be equal to the net charge or the ion.
    // Oxidation numbers are typically integers but do not have to be integers.

    // SO2: S has oxidation number 4 and O has oxidation number -2.
    // ion(Cr2O7, -2): Cr has oxidation number 6 and O has oxidation number -2.
    // ion(NH4, 1): N has oxidation number -3 and H has oxidation number 1.
    // HClO4: H has oxidation number 1 and O has oxidation number -2 and Cl has oxidation number 7.

    val ce1 = "1 Fe (s) + 1 ion(Cu, 2) (aq) --> 1 ion(Fe, 2) (aq) + 1 Cu (s)".ce // consider the oxidation state changes
    assertResult(true)(ce1.isBalanced)

    // The 4 types of redox reactions are:
    // 1. combination
    // 2. decomposition
    // 3. displacement (for example: 2 KI + 1 Cl2 --> 2 KCl + I2)
    // 4. disproportionation (for example: 2 H2O2 --> 1 H2O + 1 O2)
    // Note that not all disproportionation reactions are redox reactions.
  }

  test("combustionReactions") {
    // Specific types of redox reactions: combustion reactions.
    // Combustion reactions are reactions of hydrocarbons (species containing H and C) and oxygen, yielding CO2 and water.

    val ce1 = "1 C3H8 + 5 O2 --> 4 H2O + 3 CO2".ce // propane combustion
    assertResult(true)(ce1.isBalanced)

    val ce2 = "1 C2H4 + 3 O2 --> 2 H2O + 2 CO2".ce // ethene combustion
    assertResult(true)(ce2.isBalanced)
  }
}
