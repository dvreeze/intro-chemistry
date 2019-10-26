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

import eu.cdevreeze.introchemistry.lewis.LewisStructure
import eu.cdevreeze.introchemistry.lewis.LewisStructure.Atom
import eu.cdevreeze.introchemistry.lewis.LewisStructure.AtomKey
import eu.cdevreeze.introchemistry.lewis.LewisStructure.Bond
import eu.cdevreeze.introchemistry.typeclasses.Show

/**
 * Holder of instances of type class Show for LewisStructure etc. Inspired by Cats.
 *
 * @author Chris de Vreeze
 */
object ShowLewisStructures {

  // Below, only a few type class instances are implicit vals, for more predictable implicit resolution.

  // Show type class instance for AtomKey

  /**
   * Returns a string representation of the atom key. For example: Ca.at(1).
   */
  implicit val showAtomKey: Show[AtomKey] = Show.show { value: AtomKey =>
    value.toString
  }

  // Show type class instance for Atom

  implicit val showAtom: Show[Atom] = Show.show { value: Atom =>
    value.toString
  }

  // Show type class instance for Bond

  implicit val showBond: Show[Bond] = Show.show { value: Bond =>
    value.toString
  }

  // Show type class instance for LewisStructure

  implicit val showLewisStructure: Show[LewisStructure] = Show.show { value: LewisStructure =>
    value.builder.toString
  }
}
