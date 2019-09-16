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

import eu.cdevreeze.introchemistry.periodictable.ElementSymbol

/**
 * The Aufbau principle applied, resulting in (probable) electron configs for the different elements.
 *
 * @author Chris de Vreeze
 */
object AufbauPrinciple {

  def getProbableElectronConfigByAtomicNumber(atomicNumber: Int): ElectronConfig = electronConfigMap(atomicNumber)

  def getProbableElectronConfig(element: ElementSymbol): ElectronConfig = {
    getProbableElectronConfigByAtomicNumber(element.atomicNumber)
  }

  def getProbableAbsoluteElectronConfig(element: ElementSymbol): ElectronConfig = {
    val rawResult = getProbableElectronConfig(element)

    if (rawResult.previousNobleGasOption.isEmpty) {
      rawResult
    } else {
      // Recursive call
      val prevResult: ElectronConfig =
        getProbableAbsoluteElectronConfig(rawResult.previousNobleGasOption.get).ensuring(_.previousNobleGasOption.isEmpty)

      ElectronConfig(None, prevResult.subshellConfigs.concat(rawResult.subshellConfigs))
    }
  }

  private val orderedElectronConfigs: Seq[ElectronConfig] = Seq(
    ElectronConfig.parse("(1s1)"),
    ElectronConfig.parse("(1s2)"),
    ElectronConfig.parse("[He](2s1)"),
    ElectronConfig.parse("[He](2s2)"),
    ElectronConfig.parse("[He](2s2)(2p1)"),
    ElectronConfig.parse("[He](2s2)(2p2)"),
    ElectronConfig.parse("[He](2s2)(2p3)"),
    ElectronConfig.parse("[He](2s2)(2p4)"),
    ElectronConfig.parse("[He](2s2)(2p5)"),
    ElectronConfig.parse("[He](2s2)(2p6)"),
    ElectronConfig.parse("[Ne](3s1)"),
    ElectronConfig.parse("[Ne](3s2)"),
    ElectronConfig.parse("[Ne](3s2)(3p1)"),
    ElectronConfig.parse("[Ne](3s2)(3p2)"),
    ElectronConfig.parse("[Ne](3s2)(3p3)"),
    ElectronConfig.parse("[Ne](3s2)(3p4)"),
    ElectronConfig.parse("[Ne](3s2)(3p5)"),
    ElectronConfig.parse("[Ne](3s2)(3p6)"),
    ElectronConfig.parse("[Ar](4s1)"),
    ElectronConfig.parse("[Ar](4s2)"),
    ElectronConfig.parse("[Ar](4s2)(3d1)"),
    ElectronConfig.parse("[Ar](4s2)(3d2)"),
    ElectronConfig.parse("[Ar](4s2)(3d3)"),
    ElectronConfig.parse("[Ar](4s1)(3d5)"), // exception
    ElectronConfig.parse("[Ar](4s2)(3d5)"),
    ElectronConfig.parse("[Ar](4s2)(3d6)"),
    ElectronConfig.parse("[Ar](4s2)(3d7)"),
    ElectronConfig.parse("[Ar](4s2)(3d8)"),
    ElectronConfig.parse("[Ar](4s1)(3d10)"), // exception
    ElectronConfig.parse("[Ar](4s2)(3d10)"),
    ElectronConfig.parse("[Ar](4s2)(3d10)(4p1)"),
    ElectronConfig.parse("[Ar](4s2)(3d10)(4p2)"),
    ElectronConfig.parse("[Ar](4s2)(3d10)(4p3)"),
    ElectronConfig.parse("[Ar](4s2)(3d10)(4p4)"),
    ElectronConfig.parse("[Ar](4s2)(3d10)(4p5)"),
    ElectronConfig.parse("[Ar](4s2)(3d10)(4p6)"),
    ElectronConfig.parse("[Kr](5s1)"),
    ElectronConfig.parse("[Kr](5s2)"),
    ElectronConfig.parse("[Kr](5s2)(4d1)"),
    ElectronConfig.parse("[Kr](5s2)(4d2)"),
    ElectronConfig.parse("[Kr](5s2)(4d3)"),
    ElectronConfig.parse("[Kr](5s1)(4d5)"), // exception
    ElectronConfig.parse("[Kr](5s2)(4d5)"),
    ElectronConfig.parse("[Kr](5s2)(4d6)"),
    ElectronConfig.parse("[Kr](5s2)(4d7)"),
    ElectronConfig.parse("[Kr](5s2)(4d8)"),
    ElectronConfig.parse("[Kr](5s1)(4d10)"), // exception
    ElectronConfig.parse("[Kr](5s2)(4d10)"),
    ElectronConfig.parse("[Kr](5s2)(4d10)(5p1)"),
    ElectronConfig.parse("[Kr](5s2)(4d10)(5p2)"),
    ElectronConfig.parse("[Kr](5s2)(4d10)(5p3)"),
    ElectronConfig.parse("[Kr](5s2)(4d10)(5p4)"),
    ElectronConfig.parse("[Kr](5s2)(4d10)(5p5)"),
    ElectronConfig.parse("[Kr](5s2)(4d10)(5p6)"),
    ElectronConfig.parse("[Xe](6s1)"),
    ElectronConfig.parse("[Xe](6s2)"),
    ElectronConfig.parse("[Xe](6s2)(4f1)"), // from here downwards probably not correct!
    ElectronConfig.parse("[Xe](6s2)(4f2)"),
    ElectronConfig.parse("[Xe](6s2)(4f3)"),
    ElectronConfig.parse("[Xe](6s2)(4f4)"),
    ElectronConfig.parse("[Xe](6s2)(4f5)"),
    ElectronConfig.parse("[Xe](6s2)(4f6)"),
    ElectronConfig.parse("[Xe](6s2)(4f7)"),
    ElectronConfig.parse("[Xe](6s2)(4f8)"),
    ElectronConfig.parse("[Xe](6s2)(4f9)"),
    ElectronConfig.parse("[Xe](6s2)(4f10)"),
    ElectronConfig.parse("[Xe](6s2)(4f11)"),
    ElectronConfig.parse("[Xe](6s2)(4f12)"),
    ElectronConfig.parse("[Xe](6s2)(4f13)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d1)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d2)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d3)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d4)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d5)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d6)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d7)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d8)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d9)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d10)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d10)(6p1)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d10)(6p2)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d10)(6p3)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d10)(6p4)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d10)(6p5)"),
    ElectronConfig.parse("[Xe](6s2)(4f14)(5d10)(6p6)"),
    ElectronConfig.parse("[Rn](7s1)"),
    ElectronConfig.parse("[Rn](7s2)"),
    ElectronConfig.parse("[Rn](7s2)(5f1)"),
    ElectronConfig.parse("[Rn](7s2)(5f2)"),
    ElectronConfig.parse("[Rn](7s2)(5f3)"),
    ElectronConfig.parse("[Rn](7s2)(5f4)"),
    ElectronConfig.parse("[Rn](7s2)(5f5)"),
    ElectronConfig.parse("[Rn](7s2)(5f6)"),
    ElectronConfig.parse("[Rn](7s2)(5f7)"),
    ElectronConfig.parse("[Rn](7s2)(5f8)"),
    ElectronConfig.parse("[Rn](7s2)(5f9)"),
    ElectronConfig.parse("[Rn](7s2)(5f10)"),
    ElectronConfig.parse("[Rn](7s2)(5f11)"),
    ElectronConfig.parse("[Rn](7s2)(5f12)"),
    ElectronConfig.parse("[Rn](7s2)(5f13)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d1)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d2)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d3)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d4)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d5)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d6)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d7)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d8)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d9)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d10)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d10)(7p1)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d10)(7p2)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d10)(7p3)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d10)(7p4)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d10)(7p5)"),
    ElectronConfig.parse("[Rn](7s2)(5f14)(6d10)(7p6)"),
  )

  private val electronConfigMap: Map[Int, ElectronConfig] = {
    orderedElectronConfigs.zipWithIndex.map { case (conf, idx) => (idx + 1) -> conf }.toMap
  }
}
