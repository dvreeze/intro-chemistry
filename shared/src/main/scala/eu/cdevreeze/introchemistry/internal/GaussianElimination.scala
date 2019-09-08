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
   * Invokes method computeGaussJordanEchelonForm, but if the result has infinitely many solutions with one parameter,
   * tries to fill in that parameter and invokes computeGaussJordanEchelonForm again.
   */
  def tryToBalanceEquations(matrix: Matrix[Long], maxParameterValueToTry: Long): Matrix[Long] = {
    val rawResult = addEmptyRowsUntilColumnCountMinusOne(computeGaussJordanEchelonForm(matrix))

    if (isFoundToHaveInfinitelyManySolutionsWithOneParameter(rawResult)) {
      (1L to maxParameterValueToTry).iterator.map { parValue =>
        assert(rawResult.rows.count(_.forall(_ == 0)) == 1)

        val nextMatrix = rawResult.mapRows { row =>
          if (row.forall(_ == 0)) row.dropRight(2).appended(1L).appended(parValue) else row
        }

        computeGaussJordanEchelonForm(nextMatrix)
      }.find(isFoundToHaveExactlyOneIntegerOnlySolution).getOrElse(rawResult)
    } else {
      rawResult
    }
  }

  /**
   * Performs Gauss-Jordan elimination. In other words, solves the corresponding set of linear equations, if possible.
   * In any case, this method tries to get as far as possible, and returns the last computed matrix. To check this
   * resulting matrix, call methods such as `isFoundToHaveExactlyOneSolution` etc.
   */
  def computeGaussJordanEchelonForm(matrix: Matrix[Long]): Matrix[Long] = {
    require(hasMoreColumnsThanRows(matrix), s"Expected the column count to be at least 1 plus the row count")

    val countOfColumnsToProcess: Int = matrix.rowCount

    val topDownResult: Matrix[Long] =
      (0 until countOfColumnsToProcess).foldLeft(matrix) { case (accMatrix, colIndex) =>
        accMatrix.pipe(m => processColumnTopDown(colIndex, m))
      }

    val bottomUpResult: Matrix[Long] =
      ((countOfColumnsToProcess - 1) to 0 by -1).foldLeft(topDownResult) { case (accMatrix, colIndex) =>
        accMatrix.pipe(m => processColumnBottomUp(colIndex, m))
      }

    bottomUpResult.pipe(m => divideRows(m))
  }

  /**
   * Returns true if there is exactly one solution, even if it does not consist of whole numbers only.
   */
  def isFoundToHaveExactlyOneSolutionAllowingForFractions(matrix: Matrix[Long]): Boolean = {
    addEmptyRowsUntilColumnCountMinusOne(matrix).rows.forall { row =>
      row.init.count(_ != 0) == 1
    }
  }

  /**
   * Returns true if there is exactly one solution, consisting of whole numbers only.
   */
  def isFoundToHaveExactlyOneIntegerOnlySolution(matrix: Matrix[Long]): Boolean = {
    isFoundToHaveExactlyOneSolutionAllowingForFractions(matrix) && {
      addEmptyRowsUntilColumnCountMinusOne(matrix).rows.forall { row =>
        val coefficient = row.init.find(_ != 0).get
        coefficient != 0 && row.last % coefficient == 0
      }
    }
  }

  def isFoundToHaveNoSolutions(matrix: Matrix[Long]): Boolean = {
    addEmptyRowsUntilColumnCountMinusOne(matrix).rows.exists { row =>
      row.init.forall(_ == 0) && row.last != 0
    }
  }

  def isFoundToHaveInfinitelyManySolutions(matrix: Matrix[Long]): Boolean = {
    addEmptyRowsUntilColumnCountMinusOne(matrix).rows.exists { row =>
      row.forall(_ == 0)
    } && !isFoundToHaveNoSolutions(matrix)
  }

  def isFoundToHaveInfinitelyManySolutionsWithGivenParameterCount(matrix: Matrix[Long], parameterCount: Int): Boolean = {
    addEmptyRowsUntilColumnCountMinusOne(matrix).rows.count(_.forall(_ == 0)) == parameterCount &&
      isFoundToHaveInfinitelyManySolutions(matrix)
  }

  def isFoundToHaveInfinitelyManySolutionsWithOneParameter(matrix: Matrix[Long]): Boolean = {
    isFoundToHaveInfinitelyManySolutionsWithGivenParameterCount(matrix, 1)
  }

  def hasMoreColumnsThanRows(matrix: Matrix[Long]): Boolean = {
    matrix.columnCount > matrix.rowCount
  }

  def addEmptyRowsUntilColumnCountMinusOne(matrix: Matrix[Long]): Matrix[Long] = {
    require(hasMoreColumnsThanRows(matrix), s"Expected the column count to be at least 1 plus the row count")

    val expectedRowCount = matrix.columnCount - 1
    val countOfRowsToAdd = expectedRowCount - matrix.rowCount

    val addedRows = Seq.fill(countOfRowsToAdd)(0.until(matrix.columnCount).map(_ => 0L))

    Matrix(matrix.rows.appendedAll(addedRows)).ensuring(m => m.rowCount + 1 == m.columnCount)
  }

  /**
   * Tries to change column colIndex in such a way with operation `Matrix.addOtherMultipliedRow` that the cells below
   * cell (colIndex, colIndex) become 0. If needed, rows are swapped.
   */
  private def processColumnTopDown(colIndex: Int, matrix: Matrix[Long]): Matrix[Long] = {
    val zeroColumns = for (r <- colIndex.until(matrix.rowCount); c <- 0.until(colIndex)) yield matrix.cell(r, c)
    require(
      zeroColumns.forall(_ == 0),
      s"Expected only zeroes in columns under $colIndex in rows from $colIndex (both 0-based)")

    val startMatrix: Matrix[Long] =
      if (matrix.cell(colIndex, colIndex) == 0) {
        val rowToSwapWith = colIndex.until(matrix.rowCount).find(r => matrix.cell(r, colIndex) != 0).getOrElse(colIndex)
        matrix.swapRows(colIndex, rowToSwapWith)
      } else {
        matrix
      }

    val referenceRowIndex = colIndex
    val referenceCell = startMatrix.cell(referenceRowIndex, colIndex)

    val startRowIndex = colIndex + 1

    if (referenceCell == 0) {
      startMatrix
    } else {
      // Try to get value 0 in cells (colIndex + 1, colIndex), (colIndex + 2, colIndex) etc.

      (startRowIndex until startMatrix.rowCount).foldLeft(startMatrix) { case (accMatrix, currRowIndex) =>
        // Process row currRowIndex

        if (accMatrix.cell(currRowIndex, colIndex) == 0) {
          accMatrix
        } else {
          val factor1 = accMatrix.cell(referenceRowIndex, colIndex)
          val factor2 = -accMatrix.cell(currRowIndex, colIndex)

          accMatrix.addOtherRowMultiplyingBoth(currRowIndex, referenceRowIndex, factor1, factor2)
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
  private def processColumnBottomUp(colIndex: Int, matrix: Matrix[Long]): Matrix[Long] = {
    val startMatrix: Matrix[Long] = matrix

    val referenceRowIndex = colIndex
    val referenceCell = startMatrix.cell(referenceRowIndex, colIndex)

    val startRowIndex = colIndex - 1

    if (referenceCell == 0) {
      startMatrix
    } else {
      // Try to get value 0 in cells (rowCount - 1, colIndex), (rowCount - 2, colIndex) etc.

      (startRowIndex to 0 by -1).foldLeft(startMatrix) { case (accMatrix, currRowIndex) =>
        // Process row currRowIndex

        if (accMatrix.cell(currRowIndex, colIndex) == 0) {
          accMatrix
        } else {
          val factor1 = accMatrix.cell(referenceRowIndex, colIndex)
          val factor2 = -accMatrix.cell(currRowIndex, colIndex)

          accMatrix.addOtherRowMultiplyingBoth(currRowIndex, referenceRowIndex, factor1, factor2)
        }
      }
    }
  }

  private def divideRows(matrix: Matrix[Long]): Matrix[Long] = {
    (0 until matrix.rowCount).foldLeft(matrix) { case (accMatrix, idx) =>
      val denominatorOption = gcdOption(accMatrix.rows(idx).filter(_ != 0))

      denominatorOption.map { denominator =>
        if (denominator == 0) {
          matrix
        } else {
          Matrix.divideRow(accMatrix, idx, denominator)
        }
      }.getOrElse(matrix)
    }
  }

  private def gcdOption(xs: Seq[Long]): Option[Long] = {
    if (xs.isEmpty) {
      None
    } else {
      if (xs.sizeIs <= 2) {
        if (xs.size == 1) Some(xs.head) else Some(gcd(xs(0), xs(1)))
      } else {
        gcdOption(xs.tail).map(n => gcd(xs.head, n)) // Recursive call
      }
    }
  }

  private def gcd(x: Long, y: Long): Long = {
    if (y == 0) x else gcd(y, x % y) // Recursive call
  }
}
