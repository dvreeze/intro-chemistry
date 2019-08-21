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
  }

  test("testDivideRow") {
    val matrix = Matrix(Seq(
      Seq(BigDecimal(2), BigDecimal(-2), BigDecimal(0), BigDecimal(-6)),
      Seq(BigDecimal(1), BigDecimal(-1), BigDecimal(1), BigDecimal(1)),
      Seq(BigDecimal(0), BigDecimal(3), BigDecimal(-2), BigDecimal(-5))
    ))

    val result = matrix.divideRow(2, BigDecimal(1) / 15)

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

  test("testGuassianEliminationWorkflow") {
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
      .divideRow(0, BigDecimal(2)).tap(r => results = results.appended(r))
      .divideRow(1, BigDecimal(3)).tap(r => results = results.appended(r))

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
}
