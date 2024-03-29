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

/**
 * Spin, which is either plus half or minus half.
 *
 * @author Chris de Vreeze
 */
sealed trait Spin {

  def value: BigDecimal
}

case object PositiveSpin extends Spin {

  def value: BigDecimal = BigDecimal(1) / BigDecimal(2)
}

case object NegativeSpin extends Spin {

  def value: BigDecimal = -BigDecimal(1) / BigDecimal(2)
}
