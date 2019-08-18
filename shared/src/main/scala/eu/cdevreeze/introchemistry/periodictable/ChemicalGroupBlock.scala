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

/**
 * An chemical group block in the periodic table.
 *
 * @author Chris de Vreeze
 */
sealed trait ChemicalGroupBlock {

  def isMetalGroup: Boolean
}

object ChemicalGroupBlock {

  // TODO Describe these different groups and their properties

  case object Nonmetal extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = false
  }

  case object AlkaliMetal extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = true
  }

  case object AlkalineEarthMetal extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = true
  }

  case object TransitionMetal extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = true
  }

  case object PostTransitionMetal extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = true
  }

  case object Metalloid extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = false
  }

  case object NobleGas extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = false
  }

  case object Lanthanide extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = true
  }

  case object Actinide extends ChemicalGroupBlock {

    def isMetalGroup: Boolean = true
  }

  val values: Seq[ChemicalGroupBlock] = {
    Seq(Nonmetal, AlkaliMetal, AlkalineEarthMetal, TransitionMetal, PostTransitionMetal, Metalloid, NobleGas, Lanthanide, Actinide)
  }
}
