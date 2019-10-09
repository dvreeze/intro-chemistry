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

package eu.cdevreeze.introchemistry.typeclasses

/**
 * Type class for showing data as a string, as an alternative to the toString method (which is defined on each class).
 * Inspired by the Show type class of Cats.
 *
 * Given that we own the code base (so we can change it), putting show methods on Element, Formula, ChemicalEquation instead
 * of using this Show type class machinery would be a lot easier, of course. Nevertheless, this is a simple example that
 * allows for a comparison between the 2 approaches of extending APIs with a show method. Of course, in practice, much depends
 * on whether we own the types we would like to extend or not, and on good taste. Here we used the Show type class as if
 * we do not own the code of the above-mentioned types.
 *
 * @author Chris de Vreeze
 */
trait Show[A] {

  def show(value: A): String
}

object Show {

  def show[A](f: A => String): Show[A] = {
    f(_)
  }
}
