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

import eu.cdevreeze.introchemistry.thermochemistry.ThermochemicalEquation
import eu.cdevreeze.introchemistry.typeclasses.Show

/**
 * Holder of instances of type class Show for ThermochemicalEquation. Inspired by Cats.
 *
 * @author Chris de Vreeze
 */
object ShowThermochemicalEquations {

  // Show type class instance for ThermochemicalEquation

  implicit val showThermochemicalEquation: Show[ThermochemicalEquation] = Show.show { value: ThermochemicalEquation =>
    s"${ShowChemicalEquations.showChemicalEquation.show(value.underlyingEquation)}     dH = ${value.deltaEnthalpyInKiloJoule} kJ"
  }
}
