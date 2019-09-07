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

/**
 * Matrix, used in gaussian elimination to solve a set of linear equations.
 *
 * @author Chris de Vreeze
 */
final case class Matrix[A](rows: Seq[Seq[A]])(implicit numeric: Numeric[A]) {
  require(rows.nonEmpty, s"No rows found")
  require(rows(0).nonEmpty, s"The first row is empty, which is not allowed")
  require(rows.forall(r => r.size == rows(0).size), s"All rows must have the same number of columns")

  def rowCount: Int = rows.size

  def columnCount: Int = rows(0).size

  def cell(rowIndex: Int, columnIndex: Int): A = {
    require(rowIndex >= 0 && rowIndex < rowCount, s"Row index $rowIndex out of bounds")
    require(columnIndex >= 0 && columnIndex < columnCount, s"Column index $columnIndex out of bounds")

    rows(rowIndex)(columnIndex)
  }

  def isOnDiagonal(rowIndex: Int, columnIndex: Int): Boolean = {
    require(rowIndex >= 0 && rowIndex < rowCount, s"Row index $rowIndex out of bounds")
    require(columnIndex >= 0 && columnIndex < columnCount, s"Column index $columnIndex out of bounds")

    rowIndex == columnIndex
  }

  def swapRows(rowIndex1: Int, rowIndex2: Int): Matrix[A] = {
    require(rowIndex1 >= 0 && rowIndex1 < rowCount, s"Row index $rowIndex1 out of bounds")
    require(rowIndex2 >= 0 && rowIndex2 < rowCount, s"Row index $rowIndex2 out of bounds")

    if (rowIndex1 == rowIndex2) {
      this
    } else {
      val row1 = rows(rowIndex1)
      val row2 = rows(rowIndex2)

      Matrix(rows.updated(rowIndex1, row2).updated(rowIndex2, row1))
    }
  }

  def multiplyRow(rowIndex: Int, factor: A): Matrix[A] = {
    require(rowIndex >= 0 && rowIndex < rowCount, s"Row index $rowIndex out of bounds")

    Matrix(rows.updated(rowIndex, rows(rowIndex).map(n => numeric.times(n, factor))))
  }

  def addOtherMultipliedRow(rowIndex: Int, otherRowIndex: Int, factor: A): Matrix[A] = {
    require(rowIndex >= 0 && rowIndex < rowCount, s"Row index $rowIndex out of bounds")
    require(otherRowIndex >= 0 && otherRowIndex < rowCount, s"Row index $otherRowIndex out of bounds")
    require(otherRowIndex != rowIndex, s"The same 2 row indices are not allowed")

    val otherRowToAdd = rows(otherRowIndex).map(n => numeric.times(n, factor))
    Matrix(rows.updated(rowIndex, addRows(rows(rowIndex), otherRowToAdd)))
  }

  def addOtherRowMultiplyingBoth(rowIndex: Int, otherRowIndex: Int, factorForThisRow: A, factorForOtherRow: A): Matrix[A] = {
    require(rowIndex >= 0 && rowIndex < rowCount, s"Row index $rowIndex out of bounds")
    require(otherRowIndex >= 0 && otherRowIndex < rowCount, s"Row index $otherRowIndex out of bounds")
    require(otherRowIndex != rowIndex, s"The same 2 row indices are not allowed")

    val otherRowToAdd = rows(otherRowIndex).map(n => numeric.times(n, factorForOtherRow))
    val thisRowMultiplied = rows(rowIndex).map(n => numeric.times(n, factorForThisRow))

    Matrix(rows.updated(rowIndex, addRows(thisRowMultiplied, otherRowToAdd)))
  }

  def map[B](f: A => B)(implicit num: Numeric[B]): Matrix[B] = {
    Matrix(rows.map(row => row.map(f)))
  }

  private def addRows(row1: Seq[A], row2: Seq[A]): Seq[A] = {
    require(row1.nonEmpty, s"Row must not be empty")
    require(row1.size == row2.size, s"Rows must have the same size")

    row1.zip(row2).map { case (cell1, cell2) =>
      numeric.plus(cell1, cell2)
    }
  }
}

object Matrix {

  def divideRow(matrix: Matrix[Int], rowIndex: Int, denominator: Int): Matrix[Int] = {
    require(rowIndex >= 0 && rowIndex < matrix.rowCount, s"Row index $rowIndex out of bounds")
    require(denominator != 0, s"Division by zero not allowed")

    Matrix(matrix.rows.updated(rowIndex, matrix.rows(rowIndex).map(n => n / denominator)))
  }

  def divideRow[A](matrix: Matrix[A], rowIndex: Int, denominator: A)(implicit numeric: Fractional[A]): Matrix[A] = {
    require(rowIndex >= 0 && rowIndex < matrix.rowCount, s"Row index $rowIndex out of bounds")
    require(denominator != numeric.zero, s"Division by zero not allowed")

    Matrix(matrix.rows.updated(rowIndex, matrix.rows(rowIndex).map(n => numeric.div(n, denominator))))
  }
}
