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

import scala.collection.immutable.SeqMap
import scala.util.chaining._

/**
 * Gaussian elimination, typically performed on a matrix of n rows and n + 1 columns. This can be used to solve a set of n
 * linear equations. This, in turn, can be used to balance chemical formulas.
 *
 * @author Chris de Vreeze
 */
object GaussianElimination {

  /**
   * Invokes method computeGaussJordanEchelonForm, but if the result has infinitely many solutions with (only) one parameter,
   * tries to fill in that parameter and invokes computeGaussJordanEchelonForm again and again until either a solution has
   * been found or the search for a solution has been exhausted.
   *
   * The column count must be at least 1 plus the row count, or else an exception is thrown.
   */
  def tryToBalanceEquations(matrix: Matrix[Long], maxParameterValueToTry: Long): Matrix[Long] = {
    val rawResult = addEmptyRowsUntilColumnCountMinusOne(computeGaussJordanEchelonForm(matrix))

    if (isFoundToHaveExactlyOneNonZeroesSolution(rawResult) || isFoundToHaveNoSolutions(rawResult)) {
      rawResult
    } else {
      if (isFoundToHaveInfinitelyManySolutionsInOneParameter(rawResult)) {
        val nonConstraintRows = rawResult.rows.filter(rowContainsNoConstraint)
        assert(nonConstraintRows.sizeIs == 1, s"Expected precisely 1 non-constraint row but found ${nonConstraintRows.size} ones")

        val startMatrix = Matrix(rawResult.rows.filterNot(rowContainsNoConstraint) ++ nonConstraintRows)

        // Free variable columns, that is, the columns that are not entirely solved
        val nonSolvedColumns: Set[Int] = findAllNonSolvedColumns(startMatrix).ensuring(_.forall(_ < startMatrix.columnCount - 1))
        val sortedNonSolvedColumns: Seq[Int] = nonSolvedColumns.toSeq.sortBy(col => startMatrix.column(col).count(_ != 0L))

        val colIndex: Int = sortedNonSolvedColumns.head

        val resultMatrices: Iterator[Matrix[Long]] = 1L.until(maxParameterValueToTry).iterator.map { value =>
          val newRow = startMatrix.rows.last.updated(colIndex, 1L).updated(startMatrix.columnCount - 1, value)

          val mat: Matrix[Long] = Matrix(startMatrix.rows.init.appended(newRow))

          computeGaussJordanEchelonForm(mat)
        }

        resultMatrices.find(isFoundToHaveExactlyOneNonZeroIntegersOnlySolution).getOrElse(rawResult)
      } else {
        rawResult
      }
    }
  }

  /**
   * Invokes method computeGaussJordanEchelonForm, but if the result has infinitely many solutions with one or more parameters,
   * tries to fill in those parameters as specified and invokes computeGaussJordanEchelonForm again.
   *
   * The column count must be at least 1 plus the row count, or else an exception is thrown.
   */
  def tryToBalanceEquations(matrix: Matrix[Long], extraColumnConstraints: SeqMap[Int, Long]): Matrix[Long] = {
    // TODO Fix the implementation, and remove the println statements

    val rawResult = addEmptyRowsUntilColumnCountMinusOne(computeGaussJordanEchelonForm(matrix))

    if (isFoundToHaveExactlyOneNonZeroesSolution(rawResult) || isFoundToHaveNoSolutions(rawResult)) {
      rawResult
    } else {
      if (isFoundToHaveInfinitelyManySolutions(rawResult)) {
        val startMatrix =
          Matrix(rawResult.rows.filterNot(rowContainsNoConstraint) ++ rawResult.rows.filter(rowContainsNoConstraint))

        // Free variable columns, that is, the columns that are not entirely solved
        val nonSolvedColumns: Set[Int] = findAllNonSolvedColumns(startMatrix).ensuring(_.forall(_ < startMatrix.columnCount - 1))

        val emptyRowIndices: Seq[Int] = startMatrix.rows.zipWithIndex.filter { case (r, _) => rowContainsNoConstraint(r) }.map(_._2)
        val colIndices: Seq[Int] = nonSolvedColumns.intersect(extraColumnConstraints.keySet).toSeq.take(emptyRowIndices.size)

        println(s"Col indices: $colIndices")
        println(s"Empty row indices: $emptyRowIndices")

        if (colIndices.size == emptyRowIndices.size) {
          val mat: Matrix[Long] =
            colIndices.zip(emptyRowIndices).foldLeft(startMatrix) {
              case (accMatrix, (colIdx, rowIdx)) =>
                assert(rowContainsNoConstraint(accMatrix.rows(rowIdx)))

                val value = extraColumnConstraints.get(colIdx).ensuring(_.nonEmpty).get

                println(s"Updating row $rowIdx. Column $colIdx gets value $value")
                accMatrix.updateCell(rowIdx, colIdx, 1L).updateCell(rowIdx, accMatrix.columnCount - 1, value)
            }

          println(mat)
          computeGaussJordanEchelonForm(mat).tap(println)
        } else {
          rawResult
        }
      } else {
        rawResult
      }
    }
  }

  /**
   * Performs Gauss-Jordan elimination. In other words, solves the corresponding set of linear equations, if possible.
   * In any case, this method tries to get as far as possible, and returns the last computed matrix. To check this
   * resulting matrix, call methods such as `isFoundToHaveExactlyOneNonZeroIntegersOnlySolution` etc.
   *
   * The column count must be at least 1 plus the row count, or else an exception is thrown.
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
   * Returns true if there is exactly one (non-zeroes only) solution, even if it does not consist of whole numbers only.
   *
   * Call this method only after calling method computeGaussJordanEchelonForm (directly or indirectly), on the resulting matrix.
   */
  def isFoundToHaveExactlyOneNonZeroesSolution(matrix: Matrix[Long]): Boolean = {
    val mat = addEmptyRowsUntilColumnCountMinusOne(matrix)

    findAllNonSolvedColumns(mat).isEmpty
  }

  /**
   * Returns true if there is exactly one solution, consisting of non-zero whole numbers only.
   *
   * Call this method only after calling method computeGaussJordanEchelonForm (directly or indirectly), on the resulting matrix.
   */
  def isFoundToHaveExactlyOneNonZeroIntegersOnlySolution(matrix: Matrix[Long]): Boolean = {
    isFoundToHaveExactlyOneNonZeroesSolution(matrix) && {
      addEmptyRowsUntilColumnCountMinusOne(matrix).rows.forall { row =>
        val coefficient = row.init.find(_ != 0).get
        coefficient != 0 && row.last % coefficient == 0
      }
    }
  }

  /**
   * Returns true if there are no solutions.
   *
   * Call this method only after calling method computeGaussJordanEchelonForm (directly or indirectly), on the resulting matrix.
   */
  def isFoundToHaveNoSolutions(matrix: Matrix[Long]): Boolean = {
    addEmptyRowsUntilColumnCountMinusOne(matrix).rows.exists(rowHasNoSolutions)
  }

  /**
   * Returns true if there are infinitely many solutions.
   *
   * Call this method only after calling method computeGaussJordanEchelonForm (directly or indirectly), on the resulting matrix.
   */
  def isFoundToHaveInfinitelyManySolutions(matrix: Matrix[Long]): Boolean = {
    addEmptyRowsUntilColumnCountMinusOne(matrix).rows.exists(rowContainsNoConstraint) && !isFoundToHaveNoSolutions(matrix)
  }

  /**
   * Returns true if there are infinitely many solutions in exactly one parameter.
   *
   * Call this method only after calling method computeGaussJordanEchelonForm (directly or indirectly), on the resulting matrix.
   */
  def isFoundToHaveInfinitelyManySolutionsInOneParameter(matrix: Matrix[Long]): Boolean = {
    addEmptyRowsUntilColumnCountMinusOne(matrix).rows.count(rowContainsNoConstraint) == 1 && !isFoundToHaveNoSolutions(matrix)
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

  /**
   * Returns true if the row has precisely one (non-zero) solution (for one variable), even if it is a fraction.
   * That is, returns true if there are precisely 2 columns that are non-zero, with one of them being the last column.
   */
  private def rowHasSingleNonZeroSolution(row: Seq[Long]): Boolean = {
    row.init.count(_ != 0L) == 1 && row.last != 0L
  }

  private def findColumnIndexOfSingleNonZeroSolution(row: Seq[Long]): Option[Int] = {
    if (rowHasSingleNonZeroSolution(row)) row.zipWithIndex.find(_._1 != 0L).map(_._2) else None
  }

  private def findAllSolvedColumns(mat: Matrix[Long]): Set[Int] = {
    mat.rows.flatMap(findColumnIndexOfSingleNonZeroSolution).toSet
  }

  private def findAllNonSolvedColumns(mat: Matrix[Long]): Set[Int] = {
    0.until(mat.columnCount - 1).toSet.diff(findAllSolvedColumns(mat))
  }

  /**
   * Returns true if the row contains no constraint on the "variables".
   * That is, returns true if all columns of the row (including the last one) are zero.
   */
  private def rowContainsNoConstraint(row: Seq[Long]): Boolean = {
    row.forall(_ == 0L)
  }

  /**
   * Returns true if the row has no solutions, that is, if it contains only zeroes except for the last column, which is non-zero.
   */
  private def rowHasNoSolutions(row: Seq[Long]): Boolean = {
    row.last != 0L && row.init.forall(_ == 0L)
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
