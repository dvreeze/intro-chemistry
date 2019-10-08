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

package eu.cdevreeze.introchemistry.internal

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test for Gaussian elimination. See examples in
 * https://www.cliffsnotes.com/study-guides/algebra/linear-algebra/linear-systems/gaussian-elimination.
 *
 * @author Chris de Vreeze
 */
class GaussianEliminationTest extends AnyFunSuite {

  test("testGaussianElimination-for-simple-2-by-3-matrix") {
    val matrix = Matrix[Long](Seq(
      Seq(1, 1, 3),
      Seq(3, -2, 4)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveExactlyOneNonZeroIntegersOnlySolution(eliminationResult)
    }

    val expectedResult = Matrix[Long](Seq(
      Seq(1, 0, 2),
      Seq(0, 1, 1)
    ))

    assertResult(expectedResult) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-1") {
    val matrix = Matrix[Long](Seq(
      Seq(1, -2, 1, 0),
      Seq(2, 1, -3, 5),
      Seq(4, -7, 1, -1)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveExactlyOneNonZeroIntegersOnlySolution(eliminationResult)
    }

    val expectedResult = Matrix[Long](Seq(
      Seq(1, 0, 0, 3),
      Seq(0, 1, 0, 2),
      Seq(0, 0, 1, 1)
    ))

    assertResult(expectedResult) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-2") {
    val matrix = Matrix[Long](Seq(
      Seq(2, -2, 0, -6),
      Seq(1, -1, 1, 1),
      Seq(0, 3, -2, -5)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveExactlyOneNonZeroIntegersOnlySolution(eliminationResult)
    }

    val expectedResult = Matrix[Long](Seq(
      Seq(1, 0, 0, -2),
      Seq(0, 1, 0, 1),
      Seq(0, 0, 1, 4)
    ))

    assertResult(expectedResult) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-3") {
    val matrix = Matrix[Long](Seq(
      Seq(1, 2, 4, 23),
      Seq(1, 1, 1, 7),
      Seq(4, 2, 1, 2)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveExactlyOneNonZeroIntegersOnlySolution(eliminationResult)
    }

    val expectedResult = Matrix[Long](Seq(
      Seq(1, 0, 0, -5),
      Seq(0, 1, 0, 10),
      Seq(0, 0, 1, 2)
    ))

    assertResult(expectedResult) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-without-solutions") {
    val matrix = Matrix[Long](Seq(
      Seq(1, 1, -3, 4),
      Seq(2, 1, -1, 2),
      Seq(3, 2, -4, 7)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveNoSolutions(eliminationResult)
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-with-infinitely-many-solutions") {
    val matrix = Matrix[Long](Seq(
      Seq(1, 1, -3, 4),
      Seq(2, 1, -1, 2),
      Seq(3, 2, -4, 6)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveInfinitelyManySolutions(eliminationResult)
    }
  }

  test("testGaussianElimination-for-too-many unknowns") {
    val matrix = Matrix[Long](Seq(
      Seq(1, -1, 1, -1, 1),
      Seq(2, 1, -3, 0, 2),
      Seq(5, -2, 0, -3, 5)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveInfinitelyManySolutions(eliminationResult)
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-4") {
    // See https://chem.libretexts.org/Bookshelves/General_Chemistry/Map%3A_Chemistry_-_The_Central_Science_(Brown_et_al.)/03._Stoichiometry%3A_Calculations_with_Chemical_Formulas_and_Equations/3.1%3A_Chemical_Equations
    val matrix = Matrix[Long](Seq(
      Seq(5, 0, 0, -1, 0),
      Seq(3, 1, 0, -2, 0),
      Seq(13, 4, 1, -9, 0),
      Seq(1, 3, 2, -6, 0)
    ))

    val eliminationResult = GaussianElimination.computeGaussJordanEchelonForm(matrix)

    assertResult(true) {
      GaussianElimination.isFoundToHaveInfinitelyManySolutions(eliminationResult)
    }
  }
}
