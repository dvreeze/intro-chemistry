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

package eu.cdevreeze.introchemistry.periodictable

import ChemicalGroupBlock._

/**
 * An element in the periodic table, as just the element symbol. It also knows its atomic number and chemical group.
 *
 * @author Chris de Vreeze
 */
sealed trait ElementSymbol {

  final def symbolName: String = toString

  /**
   * Returns the atomic number of the element. Recall that the atomic number is the number of protons in one atom of this
   * element. This atomic number uniquely identifies the element, so there are no 2 different elements with the same
   * atomic number.
   *
   * Also note that in uncharged atoms the number of protons is equal to the number of electrons. The number of neutrons
   * typically varies per element, however, so one element can easily have more than 1 (stable) isotope, each having a different number of neutrons.
   */
  def atomicNumber: Int

  def chemicalGroup: ChemicalGroupBlock
}

object ElementSymbol {

  case object H extends ElementSymbol {
    def atomicNumber: Int = 1
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object He extends ElementSymbol {
    def atomicNumber: Int = 2
    def chemicalGroup: ChemicalGroupBlock = NobleGas
  }

  case object Li extends ElementSymbol {
    def atomicNumber: Int = 3
    def chemicalGroup: ChemicalGroupBlock = AlkaliMetal
  }

  case object Be extends ElementSymbol {
    def atomicNumber: Int = 4
    def chemicalGroup: ChemicalGroupBlock = AlkalineEarthMetal
  }

  case object B extends ElementSymbol {
    def atomicNumber: Int = 5
    def chemicalGroup: ChemicalGroupBlock = Metalloid
  }

  case object C extends ElementSymbol {
    def atomicNumber: Int = 6
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object N extends ElementSymbol {
    def atomicNumber: Int = 7
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object O extends ElementSymbol {
    def atomicNumber: Int = 8
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object F extends ElementSymbol {
    def atomicNumber: Int = 9
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Ne extends ElementSymbol {
    def atomicNumber: Int = 10
    def chemicalGroup: ChemicalGroupBlock = NobleGas
  }

  case object Na extends ElementSymbol {
    def atomicNumber: Int = 11
    def chemicalGroup: ChemicalGroupBlock = AlkaliMetal
  }

  case object Mg extends ElementSymbol {
    def atomicNumber: Int = 12
    def chemicalGroup: ChemicalGroupBlock = AlkalineEarthMetal
  }

  case object Al extends ElementSymbol {
    def atomicNumber: Int = 13
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Si extends ElementSymbol {
    def atomicNumber: Int = 14
    def chemicalGroup: ChemicalGroupBlock = Metalloid
  }

  case object P extends ElementSymbol {
    def atomicNumber: Int = 15
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object S extends ElementSymbol {
    def atomicNumber: Int = 16
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Cl extends ElementSymbol {
    def atomicNumber: Int = 17
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Ar extends ElementSymbol {
    def atomicNumber: Int = 18
    def chemicalGroup: ChemicalGroupBlock = NobleGas
  }

  case object K extends ElementSymbol {
    def atomicNumber: Int = 19
    def chemicalGroup: ChemicalGroupBlock = AlkaliMetal
  }

  case object Ca extends ElementSymbol {
    def atomicNumber: Int = 20
    def chemicalGroup: ChemicalGroupBlock = AlkalineEarthMetal
  }

  case object Sc extends ElementSymbol {
    def atomicNumber: Int = 21
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ti extends ElementSymbol {
    def atomicNumber: Int = 22
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object V extends ElementSymbol {
    def atomicNumber: Int = 23
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Cr extends ElementSymbol {
    def atomicNumber: Int = 24
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Mn extends ElementSymbol {
    def atomicNumber: Int = 25
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Fe extends ElementSymbol {
    def atomicNumber: Int = 26
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Co extends ElementSymbol {
    def atomicNumber: Int = 27
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ni extends ElementSymbol {
    def atomicNumber: Int = 28
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Cu extends ElementSymbol {
    def atomicNumber: Int = 29
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Zn extends ElementSymbol {
    def atomicNumber: Int = 30
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ga extends ElementSymbol {
    def atomicNumber: Int = 31
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Ge extends ElementSymbol {
    def atomicNumber: Int = 32
    def chemicalGroup: ChemicalGroupBlock = Metalloid
  }

  case object As extends ElementSymbol {
    def atomicNumber: Int = 33
    def chemicalGroup: ChemicalGroupBlock = Metalloid
  }

  case object Se extends ElementSymbol {
    def atomicNumber: Int = 34
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Br extends ElementSymbol {
    def atomicNumber: Int = 35
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Kr extends ElementSymbol {
    def atomicNumber: Int = 36
    def chemicalGroup: ChemicalGroupBlock = NobleGas
  }

  case object Rb extends ElementSymbol {
    def atomicNumber: Int = 37
    def chemicalGroup: ChemicalGroupBlock = AlkaliMetal
  }

  case object Sr extends ElementSymbol {
    def atomicNumber: Int = 38
    def chemicalGroup: ChemicalGroupBlock = AlkalineEarthMetal
  }

  case object Y extends ElementSymbol {
    def atomicNumber: Int = 39
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Zr extends ElementSymbol {
    def atomicNumber: Int = 40
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Nb extends ElementSymbol {
    def atomicNumber: Int = 41
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Mo extends ElementSymbol {
    def atomicNumber: Int = 42
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Tc extends ElementSymbol {
    def atomicNumber: Int = 43
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ru extends ElementSymbol {
    def atomicNumber: Int = 44
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Rh extends ElementSymbol {
    def atomicNumber: Int = 45
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Pd extends ElementSymbol {
    def atomicNumber: Int = 46
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ag extends ElementSymbol {
    def atomicNumber: Int = 47
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Cd extends ElementSymbol {
    def atomicNumber: Int = 48
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object In extends ElementSymbol {
    def atomicNumber: Int = 49
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Sn extends ElementSymbol {
    def atomicNumber: Int = 50
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Sb extends ElementSymbol {
    def atomicNumber: Int = 51
    def chemicalGroup: ChemicalGroupBlock = Metalloid
  }

  case object Te extends ElementSymbol {
    def atomicNumber: Int = 52
    def chemicalGroup: ChemicalGroupBlock = Metalloid
  }

  case object I extends ElementSymbol {
    def atomicNumber: Int = 53
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Xe extends ElementSymbol {
    def atomicNumber: Int = 54
    def chemicalGroup: ChemicalGroupBlock = NobleGas
  }

  case object Cs extends ElementSymbol {
    def atomicNumber: Int = 55
    def chemicalGroup: ChemicalGroupBlock = AlkaliMetal
  }

  case object Ba extends ElementSymbol {
    def atomicNumber: Int = 56
    def chemicalGroup: ChemicalGroupBlock = AlkalineEarthMetal
  }

  case object La extends ElementSymbol {
    def atomicNumber: Int = 57
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Ce extends ElementSymbol {
    def atomicNumber: Int = 58
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Pr extends ElementSymbol {
    def atomicNumber: Int = 59
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Nd extends ElementSymbol {
    def atomicNumber: Int = 60
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Pm extends ElementSymbol {
    def atomicNumber: Int = 61
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Sm extends ElementSymbol {
    def atomicNumber: Int = 62
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Eu extends ElementSymbol {
    def atomicNumber: Int = 63
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Gd extends ElementSymbol {
    def atomicNumber: Int = 64
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Tb extends ElementSymbol {
    def atomicNumber: Int = 65
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Dy extends ElementSymbol {
    def atomicNumber: Int = 66
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Ho extends ElementSymbol {
    def atomicNumber: Int = 67
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Er extends ElementSymbol {
    def atomicNumber: Int = 68
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Tm extends ElementSymbol {
    def atomicNumber: Int = 69
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Yb extends ElementSymbol {
    def atomicNumber: Int = 70
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Lu extends ElementSymbol {
    def atomicNumber: Int = 71
    def chemicalGroup: ChemicalGroupBlock = Lanthanide
  }

  case object Hf extends ElementSymbol {
    def atomicNumber: Int = 72
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ta extends ElementSymbol {
    def atomicNumber: Int = 73
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object W extends ElementSymbol {
    def atomicNumber: Int = 74
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Re extends ElementSymbol {
    def atomicNumber: Int = 75
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Os extends ElementSymbol {
    def atomicNumber: Int = 76
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ir extends ElementSymbol {
    def atomicNumber: Int = 77
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Pt extends ElementSymbol {
    def atomicNumber: Int = 78
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Au extends ElementSymbol {
    def atomicNumber: Int = 79
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Hg extends ElementSymbol {
    def atomicNumber: Int = 80
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Tl extends ElementSymbol {
    def atomicNumber: Int = 81
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Pb extends ElementSymbol {
    def atomicNumber: Int = 82
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Bi extends ElementSymbol {
    def atomicNumber: Int = 83
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Po extends ElementSymbol {
    def atomicNumber: Int = 84
    def chemicalGroup: ChemicalGroupBlock = Metalloid
  }

  case object At extends ElementSymbol {
    def atomicNumber: Int = 85
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Rn extends ElementSymbol {
    def atomicNumber: Int = 86
    def chemicalGroup: ChemicalGroupBlock = NobleGas
  }

  case object Fr extends ElementSymbol {
    def atomicNumber: Int = 87
    def chemicalGroup: ChemicalGroupBlock = AlkaliMetal
  }

  case object Ra extends ElementSymbol {
    def atomicNumber: Int = 88
    def chemicalGroup: ChemicalGroupBlock = AlkalineEarthMetal
  }

  case object Ac extends ElementSymbol {
    def atomicNumber: Int = 89
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Th extends ElementSymbol {
    def atomicNumber: Int = 90
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Pa extends ElementSymbol {
    def atomicNumber: Int = 91
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object U extends ElementSymbol {
    def atomicNumber: Int = 92
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Np extends ElementSymbol {
    def atomicNumber: Int = 93
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Pu extends ElementSymbol {
    def atomicNumber: Int = 94
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Am extends ElementSymbol {
    def atomicNumber: Int = 95
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Cm extends ElementSymbol {
    def atomicNumber: Int = 96
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Bk extends ElementSymbol {
    def atomicNumber: Int = 97
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Cf extends ElementSymbol {
    def atomicNumber: Int = 98
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Es extends ElementSymbol {
    def atomicNumber: Int = 99
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Fm extends ElementSymbol {
    def atomicNumber: Int = 100
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Md extends ElementSymbol {
    def atomicNumber: Int = 101
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object No extends ElementSymbol {
    def atomicNumber: Int = 102
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Lr extends ElementSymbol {
    def atomicNumber: Int = 103
    def chemicalGroup: ChemicalGroupBlock = Actinide
  }

  case object Rf extends ElementSymbol {
    def atomicNumber: Int = 104
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Db extends ElementSymbol {
    def atomicNumber: Int = 105
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Sg extends ElementSymbol {
    def atomicNumber: Int = 106
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Bh extends ElementSymbol {
    def atomicNumber: Int = 107
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Hs extends ElementSymbol {
    def atomicNumber: Int = 108
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Mt extends ElementSymbol {
    def atomicNumber: Int = 109
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Ds extends ElementSymbol {
    def atomicNumber: Int = 110
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Rg extends ElementSymbol {
    def atomicNumber: Int = 111
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Cn extends ElementSymbol {
    def atomicNumber: Int = 112
    def chemicalGroup: ChemicalGroupBlock = TransitionMetal
  }

  case object Nh extends ElementSymbol {
    def atomicNumber: Int = 113
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Fl extends ElementSymbol {
    def atomicNumber: Int = 114
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Mc extends ElementSymbol {
    def atomicNumber: Int = 115
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Lv extends ElementSymbol {
    def atomicNumber: Int = 116
    def chemicalGroup: ChemicalGroupBlock = PostTransitionMetal
  }

  case object Ts extends ElementSymbol {
    def atomicNumber: Int = 117
    def chemicalGroup: ChemicalGroupBlock = Nonmetal
  }

  case object Og extends ElementSymbol {
    def atomicNumber: Int = 118
    def chemicalGroup: ChemicalGroupBlock = NobleGas
  }

  val elementsByPeriod: Seq[Seq[ElementSymbol]] = {
    Seq(
      Seq(H, He),
      Seq(Li, Be, B, C, N, O, F, Ne),
      Seq(Na, Mg, Al, Si, P, S, Cl, Ar),
      Seq(K, Ca, Sc, Ti, V, Cr, Mn, Fe, Co, Ni, Cu, Zn, Ga, Ge, As, Se, Br, Kr),
      Seq(Rb, Sr, Y, Zr, Nb, Mo, Tc, Ru, Rh, Pd, Ag, Cd, In, Sn, Sb, Te, I, Xe),
      Seq(Cs, Ba, La, Ce, Pr, Nd, Pm, Sm, Eu, Gd, Tb, Dy, Ho, Er, Tm, Yb, Lu, Hf, Ta, W, Re, Os, Ir, Pt, Au, Hg, Tl, Pb, Bi, Po, At, Rn),
      Seq(Fr, Ra, Ac, Th, Pa, U, Np, Pu, Am, Cm, Bk, Cf, Es, Fm, Md, No, Lr, Rf, Db, Sg, Bh, Hs, Mt, Ds, Rg, Cn, Nh, Fl, Mc, Lv, Ts, Og)
    )
  }

  val allElements: Seq[ElementSymbol] = {
    elementsByPeriod.flatten.ensuring(_.sizeIs == 118).ensuring(_.distinct.sizeIs == 118)
  }

  val elementsByAtomicNumber: Map[Int, ElementSymbol] = {
    allElements.groupBy(_.atomicNumber).view.mapValues(_.ensuring(_.sizeIs == 1).head).toMap
      .ensuring(_.sizeIs == allElements.size)
  }

  val halogens: Set[ElementSymbol] = Set(F, Cl, Br, I, At, Ts)

  val metals: Set[ElementSymbol] = allElements.filter(_.chemicalGroup.isMetalGroup).toSet

  /**
   * Returns the 7 diatomic elements as element symbols.
   *
   * See https://www.thoughtco.com/what-are-the-seven-diatomic-elements-606623:
   * "Have No Fear Of Ice Cold Beer"
   */
  val diatomicElements: Set[ElementSymbol] = Set(H, N, F, O, I, Cl, Br)

  def apply(symbolName: String): ElementSymbol = parse(symbolName)

  def parse(symbolName: String): ElementSymbol = {
    allElements.find(_.symbolName.toLowerCase == symbolName.toLowerCase).getOrElse(sys.error(s"Missing element with symbol name $symbolName"))
  }

  def fromAtomicNumber(atomicNumber: Int): ElementSymbol = {
    elementsByAtomicNumber.getOrElse(atomicNumber, sys.error(s"Missing element with atomic number $atomicNumber"))
  }
}
