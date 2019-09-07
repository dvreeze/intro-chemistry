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

import scala.util.chaining._

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test for matrix operations.
 *
 * @author Chris de Vreeze
 */
class MatrixTest extends AnyFunSuite {

  test("testSwapRows") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    val result = matrix.swapRows(1, 2)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1))
    ))

    assertResult(expectedResult) {
      result
    }
  }

  test("testAddOtherMultipliedRow") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    val result = matrix.addOtherMultipliedRow(1, 0, BigDecimal(-1) / 2)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    assertResult(expectedResult) {
      result
    }

    val result2 = matrix.addOtherRowMultiplyingBoth(1, 0, 1, BigDecimal(-1) / 2)

    assertResult(expectedResult) {
      result2
    }
  }

  test("testDivideRow") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    val result = Matrix.divideRow(matrix, 2, BigDecimal(1) / 15)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(45), BigDecimal(-30), BigDecimal(-75))
    ))

    assertResult(expectedResult) {
      result
    }
  }

  test("testMap") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    val result = matrix.map(n => n * 2)

    val expectedResult = Matrix(Seq(
      Seq(BigDecimal(4), BigDecimal(-4), BigDecimal(0), BigDecimal(-12)),
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(2), BigDecimal(2)),
      Seq(BigDecimal(0), BigDecimal(6), BigDecimal(-4), BigDecimal(-10))
    ))

    assertResult(expectedResult) {
      result
    }
  }

  test("testGaussianEliminationWorkflow") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    var results: Seq[Matrix[BigDecimal]] = Seq.empty

    matrix
      .addOtherMultipliedRow(1, 0, BigDecimal(-1) / 2).tap(r => results = results.appended(r))
      .swapRows(1, 2).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(1, 2, BigDecimal(2)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(0, 1, BigDecimal(2) / 3).tap(r => results = results.appended(r))
      .pipe(m => Matrix.divideRow(m, 0, BigDecimal(2))).tap(r => results = results.appended(r))
      .pipe(m => Matrix.divideRow(m, 1, BigDecimal(3))).tap(r => results = results.appended(r))

    val expectedResults = Seq(
      Matrix(Seq(
        Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4)),
        Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
      )),
      Matrix(Seq(
        Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
        Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4))
      )),
      Matrix(Seq(
        Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
        Seq(BigDecimal(0), BigDecimal(3), BigDecimal(0), BigDecimal(3)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4))
      )),
      Matrix(Seq(
        Seq(BigDecimal(2), BigDecimal(0), BigDecimal(0), BigDecimal(-4)),
        Seq(BigDecimal(0), BigDecimal(3), BigDecimal(0), BigDecimal(3)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4))
      )),
      Matrix(Seq(
        Seq(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(-2)),
        Seq(BigDecimal(0), BigDecimal(3), BigDecimal(0), BigDecimal(3)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4))
      )),
      Matrix(Seq(
        Seq(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(-2)),
        Seq(BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(1)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(4))
      )),
    )

    assertResult(expectedResults) {
      results
    }
  }

  test("testOtherGaussianEliminationWorkflow") {
    // See https://chem.libretexts.org/Bookshelves/General_Chemistry/Map%3A_Chemistry_-_The_Central_Science_(Brown_et_al.)/03._Stoichiometry%3A_Calculations_with_Chemical_Formulas_and_Equations/3.1%3A_Chemical_Equations
    val matrix = Matrix(Seq(
      Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
      Seq(BigDecimal(3), BigDecimal(1), BigDecimal(0), BigDecimal(-2), BigDecimal(0)),
      Seq(BigDecimal(13), BigDecimal(4), BigDecimal(1), BigDecimal(-9), BigDecimal(0)),
      Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
    ))

    var results: Seq[Matrix[BigDecimal]] = Seq.empty

    matrix
      .multiplyRow(1, BigDecimal(5)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(1, 0, BigDecimal(-3)).tap(r => results = results.appended(r))
      .multiplyRow(2, BigDecimal(5)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(2, 0, BigDecimal(-13)).tap(r => results = results.appended(r))
      .multiplyRow(3, BigDecimal(5)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(3, 0, BigDecimal(-1)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(2, 1, BigDecimal(-4)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(3, 1, BigDecimal(-3)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(3, 2, BigDecimal(-2)).tap(r => results = results.appended(r))

    val expectedResults = Seq(
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(15), BigDecimal(5), BigDecimal(0), BigDecimal(-10), BigDecimal(0)),
        Seq(BigDecimal(13), BigDecimal(4), BigDecimal(1), BigDecimal(-9), BigDecimal(0)),
        Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(13), BigDecimal(4), BigDecimal(1), BigDecimal(-9), BigDecimal(0)),
        Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(65), BigDecimal(20), BigDecimal(5), BigDecimal(-45), BigDecimal(0)),
        Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(20), BigDecimal(5), BigDecimal(-32), BigDecimal(0)),
        Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(20), BigDecimal(5), BigDecimal(-32), BigDecimal(0)),
        Seq(BigDecimal(5), BigDecimal(15), BigDecimal(10), BigDecimal(-30), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(20), BigDecimal(5), BigDecimal(-32), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(15), BigDecimal(10), BigDecimal(-29), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(5), BigDecimal(-4), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(15), BigDecimal(10), BigDecimal(-29), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(5), BigDecimal(-4), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(10), BigDecimal(-8), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(5), BigDecimal(-4), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0))
      )),
    )

    assertResult(expectedResults) {
      results
    }
  }

  test("testOtherGaussianEliminationWorkflowAgain") {
    // See https://chem.libretexts.org/Bookshelves/General_Chemistry/Map%3A_Chemistry_-_The_Central_Science_(Brown_et_al.)/03._Stoichiometry%3A_Calculations_with_Chemical_Formulas_and_Equations/3.1%3A_Chemical_Equations
    val matrix = Matrix(Seq(
      Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
      Seq(BigDecimal(3), BigDecimal(1), BigDecimal(0), BigDecimal(-2), BigDecimal(0)),
      Seq(BigDecimal(13), BigDecimal(4), BigDecimal(1), BigDecimal(-9), BigDecimal(0)),
      Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
    ))

    var results: Seq[Matrix[BigDecimal]] = Seq.empty

    matrix
      .addOtherRowMultiplyingBoth(1, 0, BigDecimal(5), BigDecimal(-3)).tap(r => results = results.appended(r))
      .addOtherRowMultiplyingBoth(2, 0, BigDecimal(5), BigDecimal(-13)).tap(r => results = results.appended(r))
      .addOtherRowMultiplyingBoth(3, 0, BigDecimal(5), BigDecimal(-1)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(2, 1, BigDecimal(-4)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(3, 1, BigDecimal(-3)).tap(r => results = results.appended(r))
      .addOtherMultipliedRow(3, 2, BigDecimal(-2)).tap(r => results = results.appended(r))

    val expectedResults = Seq(
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(13), BigDecimal(4), BigDecimal(1), BigDecimal(-9), BigDecimal(0)),
        Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(20), BigDecimal(5), BigDecimal(-32), BigDecimal(0)),
        Seq(BigDecimal(1), BigDecimal(3), BigDecimal(2), BigDecimal(-6), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(20), BigDecimal(5), BigDecimal(-32), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(15), BigDecimal(10), BigDecimal(-29), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(5), BigDecimal(-4), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(15), BigDecimal(10), BigDecimal(-29), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(5), BigDecimal(-4), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(10), BigDecimal(-8), BigDecimal(0))
      )),
      Matrix(Seq(
        Seq(BigDecimal(5), BigDecimal(0), BigDecimal(0), BigDecimal(-1), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(5), BigDecimal(0), BigDecimal(-7), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(5), BigDecimal(-4), BigDecimal(0)),
        Seq(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0))
      )),
    )

    assertResult(expectedResults) {
      results
    }
  }
}
