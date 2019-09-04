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

/**
 * Gaussian elimination, typically performed on a matrix of n rows and n + 1 columns. This can be used to solve a set of n
 * linear equations. This, in turn, can be used to balance chemical formulas.
 *
 * @author Chris de Vreeze
 */
object GaussianElimination {

  /**
   * Performs Gauss-Jordan elimination. In other words, solves the corresponding set of linear equations, if possible.
   * In any case, this method tries to get as far as possible, and returns the last computed matrix. To check this
   * resulting matrix, call methods such as `isFoundToHaveExactlyOneSolution` etc.
   */
  def computeGaussJordanEchelonForm[A](matrix: Matrix[A])(implicit numeric: Fractional[A]): Matrix[A] = {
    require(matrix.columnCount >= 1 + matrix.rowCount, s"Expected the column count to be at least 1 plus the row count")

    val countOfColumnsToProcess: Int = matrix.rowCount

    val topDownResult: Matrix[A] =
      (0 until countOfColumnsToProcess).foldLeft(matrix) { case (accMatrix, colIndex) =>
        accMatrix.pipe(m => processColumnTopDown(colIndex, m))
      }

    val bottomUpResult: Matrix[A] =
      ((countOfColumnsToProcess - 1) to 0 by -1).foldLeft(topDownResult) { case (accMatrix, colIndex) =>
        accMatrix.pipe(m => processColumnBottomUp(colIndex, m))
      }

    bottomUpResult.pipe(m => divideRows(m))
  }

  def isFoundToHaveExactlyOneSolution[A](matrix: Matrix[A])(implicit numeric: Fractional[A]): Boolean = {
    matrix.rows.forall { row =>
      row.init.count(_ != numeric.zero) == 1
    }
  }

  def isFoundToHaveNoSolutions[A](matrix: Matrix[A])(implicit numeric: Fractional[A]): Boolean = {
    matrix.rows.exists { row =>
      row.init.forall(_ == numeric.zero) && row.last != numeric.zero
    }
  }

  def isFoundToHaveInfinitelyManySolutions[A](matrix: Matrix[A])(implicit numeric: Fractional[A]): Boolean = {
    matrix.rows.exists { row =>
      row.forall(_ == numeric.zero)
    } && !isFoundToHaveNoSolutions(matrix)
  }

  /**
   * Tries to change column colIndex in such a way with operation `Matrix.addOtherMultipliedRow` that the cells below
   * cell (colIndex, colIndex) become 0. If needed, rows are swapped.
   */
  private def processColumnTopDown[A](colIndex: Int, matrix: Matrix[A])(implicit numeric: Fractional[A]): Matrix[A] = {
    val zeroColumns = for (r <- colIndex.until(matrix.rowCount); c <- 0.until(colIndex)) yield matrix.cell(r, c)
    require(
      zeroColumns.forall(_ == numeric.zero),
      s"Expected only zeroes in columns under $colIndex in rows from $colIndex (both 0-based)")

    val startMatrix: Matrix[A] =
      if (matrix.cell(colIndex, colIndex) == numeric.zero) {
        val rowToSwapWith = colIndex.until(matrix.rowCount).find(r => matrix.cell(r, colIndex) != numeric.zero).getOrElse(colIndex)
        matrix.swapRows(colIndex, rowToSwapWith)
      } else {
        matrix
      }

    val referenceRowIndex = colIndex
    val referenceCell = startMatrix.cell(referenceRowIndex, colIndex)

    val startRowIndex = colIndex + 1

    if (referenceCell == numeric.zero) {
      startMatrix
    } else {
      // Try to get value 0 in cells (colIndex + 1, colIndex), (colIndex + 2, colIndex) etc.

      (startRowIndex until startMatrix.rowCount).foldLeft(startMatrix) { case (accMatrix, currRowIndex) =>
        // Process row currRowIndex

        if (accMatrix.cell(currRowIndex, colIndex) == numeric.zero) {
          accMatrix
        } else {
          val factor = numeric.negate(numeric.div(accMatrix.cell(currRowIndex, colIndex), referenceCell))
          accMatrix.addOtherMultipliedRow(currRowIndex, referenceRowIndex, factor)
        }
      }
    }
  }

  /**
   * Tries to change column colIndex in such a way with operation `Matrix.addOtherMultipliedRow` that the cells above
   * cell (colIndex, colIndex) become 0. No rows are swapped, because this method is called after method processColumnTopDown,
   * which makes row swapping no longer feasible without breaking the elimination process. If this workflow does not proceed
   * "normally", the resulting matrix at that point is returned.
   */
  private def processColumnBottomUp[A](colIndex: Int, matrix: Matrix[A])(implicit numeric: Fractional[A]): Matrix[A] = {
    val startMatrix: Matrix[A] = matrix

    val referenceRowIndex = colIndex
    val referenceCell = startMatrix.cell(referenceRowIndex, colIndex)

    val startRowIndex = colIndex - 1

    if (referenceCell == numeric.zero) {
      startMatrix
    } else {
      // Try to get value 0 in cells (rowCount - 1, colIndex), (rowCount - 2, colIndex) etc.

      (startRowIndex to 0 by -1).foldLeft(startMatrix) { case (accMatrix, currRowIndex) =>
        // Process row currRowIndex

        if (accMatrix.cell(currRowIndex, colIndex) == numeric.zero) {
          accMatrix
        } else {
          val factor = numeric.negate(numeric.div(accMatrix.cell(currRowIndex, colIndex), referenceCell))
          accMatrix.addOtherMultipliedRow(currRowIndex, referenceRowIndex, factor)
        }
      }
    }
  }

  private def divideRows[A](matrix: Matrix[A])(implicit numeric: Fractional[A]): Matrix[A] = {
    (0 until matrix.rowCount).foldLeft(matrix) { case (accMatrix, idx) =>
      val denominator = accMatrix.cell(idx, idx)

      if (denominator == numeric.zero) {
        matrix
      } else {
        accMatrix.divideRow(idx, denominator)
      }
    }
  }
}
