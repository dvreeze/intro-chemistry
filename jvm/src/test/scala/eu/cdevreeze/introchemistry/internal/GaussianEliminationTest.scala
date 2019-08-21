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
    val matrix = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(1), BigDecimal(3)),
      Seq(BigDecimal(3), BigDecimal(-2), BigDecimal(4))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(0), BigDecimal(2)),
      Seq(BigDecimal(0), BigDecimal(1), BigDecimal(1))
    ))

    assertResult(Some(expectedResult)) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-1") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(-2), BigDecimal(1), BigDecimal(0)),
      Seq(BigDecimal(2), BigDecimal(1), BigDecimal(-3), BigDecimal(5)),
      Seq(BigDecimal(4), BigDecimal(-7), BigDecimal(1), BigDecimal(-1))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(3)),
      Seq(BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(2)),
      Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(1))
    ))

    assertResult(Some(expectedResult)) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-2") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(-2)),
      Seq(BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4))
    ))

    assertResult(Some(expectedResult)) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-3") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(2), BigDecimal(4), BigDecimal(23)),
      Seq(BigDecimal(1), BigDecimal(1), BigDecimal(1), BigDecimal(7)),
      Seq(BigDecimal(4), BigDecimal(2), BigDecimal(1), BigDecimal(2))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(-5)),
      Seq(BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(10)),
      Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(2))
    ))

    assertResult(Some(expectedResult.map(n => BigDecimal(n.toInt)))) {
      eliminationResult.map(m => m.map(n => BigDecimal(n.toInt)))
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-without-solutions") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(1), BigDecimal(-3), BigDecimal(4)),
      Seq(BigDecimal(2), BigDecimal(1), BigDecimal(-1), BigDecimal(2)),
      Seq(BigDecimal(3), BigDecimal(2), BigDecimal(-4), BigDecimal(7))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    assertResult(None) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-simple-3-by-4-matrix-with-infinitely-many-solutions") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(1), BigDecimal(-3), BigDecimal(4)),
      Seq(BigDecimal(2), BigDecimal(1), BigDecimal(-1), BigDecimal(2)),
      Seq(BigDecimal(3), BigDecimal(2), BigDecimal(-4), BigDecimal(6))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    assertResult(None) {
      eliminationResult
    }
  }

  test("testGaussianElimination-for-too-many unknowns") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(-1), BigDecimal(1)),
      Seq(BigDecimal(2), BigDecimal(1), BigDecimal(-3), BigDecimal(0), BigDecimal(2)),
      Seq(BigDecimal(5), BigDecimal(-2), BigDecimal(0), BigDecimal(-3), BigDecimal(5))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    assertResult(None) {
      eliminationResult
    }
  }

  /*
  test("testGaussianElimination-for-simple-3-by-4-matrix-4") {
    // See https://chem.libretexts.org/Bookshelves/General_Chemistry/Map%3A_Chemistry_-_The_Central_Science_(Brown_et_al.)/03._Stoichiometry%3A_Calculations_with_Chemical_Formulas_and_Equations/3.1%3A_Chemical_Equations
    val matrix = Matrix(Seq(
      Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
      Seq(BigDecimal(3), BigDecimal(1), BigDecimal(0), BigDecimal(-2), BigDecimal(0)),
      Seq(BigDecimal(13), BigDecimal(4), BigDecimal(1), BigDecimal(-9), BigDecimal(0)),
      Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
    ))

    val eliminationResult = GaussianElimination.findGaussJordanEchelonForm(matrix)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(7)),
      Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(4)),
      Seq(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(5))
    ))

    assertResult(Some(expectedResult)) {
      eliminationResult
    }
  }
  */
}
