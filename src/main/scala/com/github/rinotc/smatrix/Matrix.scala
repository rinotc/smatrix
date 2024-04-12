package com.github.rinotc.smatrix

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.boundary
import scala.util.boundary.break

/**
 * 行列
 *
 * @param rows
 *   行数 (行数は0より大きい)
 * @param cols
 *   列数 (列数は0より大きい)
 */
final class Matrix[N: Numeric: ClassTag](val rows: Int, val cols: Int) {
  require(rows > 0, "The number of rows must be greater than 0")
  require(cols > 0, "The number of columns must be greater than 0")

  private val data: Array[Array[N]] = Array.ofDim[N](rows, cols)

  def apply(row: Int): Array[N] = data(row)

  def apply(row: Int, col: Int): N = data(row)(col)

  def update(row: Int, col: Int, value: N): Unit = {
    data(row)(col) = value
  }

  def update(row: Int, value: Array[N]): Unit = {
    data(row) = value
  }

  @targetName("timesSymbol")
  def *(that: Matrix[N]): Matrix[N] = times(that)

  @targetName("timesSymbol")
  def *(that: N): Matrix[N] = times(that)

  @targetName("plusSymbol")
  def +(that: Matrix[N]): Matrix[N] = plus(that)

  /**
   * 行列の乗法
   *
   * A * B = (a_ij) * (b_ij) = (a_i1 * b_1j + a_i2 * b_2j + ... + a_in * b_nj)
   *
   * @param that
   *   乗法する行列 (this.cols == that.rows である必要がある)
   */
  def times(that: Matrix[N])(using num: Numeric[N]): Matrix[N] = {
    require(
      this.cols == that.rows, // 2 x 3 * 3 x 5 => 2 x 5
      "The number of columns of the first matrix must be equal to the number of rows of the second matrix"
    )
    val result = new Matrix(this.rows, that.cols)
    for {
      i <- 0 until this.rows
      j <- 0 until that.cols
    } {
      var sum: N = num.zero
      for (k <- 0 until this.cols) {
        sum = num.plus(sum, num.times(this(i, k), that(k, j)))
      }
      result(i, j) = sum
    }
    result
  }

  /**
   * スカラー乗法
   *
   * c * A = c * (a_ij) = (c * a_ij)
   *
   * @param that
   *   スカラー値
   */
  def times(that: N)(using num: Numeric[N]): Matrix[N] = {
    val result = new Matrix(this.rows, this.cols)
    for {
      i <- 0 until this.rows
      j <- 0 until this.cols
    } {
      result(i, j) = num.times(this(i, j), that)
    }
    result
  }

  /**
   * 行列の加算
   *
   * A + B = (a_ij) + (b_ij) = (a_ij + b_ij)
   *
   * @param that
   *   加算する行列
   */
  def plus(that: Matrix[N])(using num: Numeric[N]): Matrix[N] = {
    require(
      this.shape == that.shape,
      "The shape of the two matrices must be the same"
    )
    val result = new Matrix(this.rows, this.cols)
    for {
      i <- 0 until this.rows
      j <- 0 until this.cols
    } {
      result(i, j) = num.plus(this(i, j), that(i, j))
    }
    result
  }

  /**
   * 行列のトレース（=対角成分の和）
   *
   * `tr(A) = a_11 + a_22 + ... + a_nn `
   *
   * @note
   *   行列のトレースは正方行列のときのみ可能
   * @return
   *   トレース
   */
  def trace(using num: Numeric[N]): N = {
    require(
      this.isSquare,
      "The matrix must be square"
    )
    var sum: N = num.zero
    for {
      i <- 0 until this.rows
    } {
      sum = num.plus(sum, this(i, i))
    }
    sum
  }

  /**
   * 行列の転置
   *
   * @return
   *   転置行列
   */
  def transpose: Matrix[N] = {
    val result = new Matrix(this.cols, this.rows)
    for {
      i <- 0 until this.rows
      j <- 0 until this.cols
    } {
      result(j, i) = this(i, j)
    }
    result
  }

  /**
   * 行列のランク
   */
  def rank: Int = {
    val m    = this.toDouble // 元の行列を変更しないためにコピーを作成
    val eps  = 1e-10
    var rank = 0
    for (col <- 0 until m.cols) {
      var pivotRow: Int = -1
      boundary {
        for (row <- rank until m.rows) {
          if (math.abs(m(row, col)) > eps) {
            pivotRow = row
            break()
          }
        }
      }

      if (pivotRow != -1) {
        // ピボット行を交換
        val tmp = m(pivotRow)
        m(pivotRow) = m(rank)
        m(rank) = tmp

        val pivot = m(rank, col)
        // ピボット行をピボットで割る
        for (j <- 0 until m.cols) {
          m(rank, j) /= pivot
        }

        // ピボット列の他のすべての要素を0にする
        for (i <- 0 until m.rows) {
          if (i != rank) {
            val factor = m(i, col)
            for (j <- 0 until m.cols) {
              m(i, j) -= factor * m(rank, j)
            }
          }
        }

        rank += 1
      }
    }
    rank
  }

  /** フルランクかどうか？ */
  def isFullRank: Boolean = rank == math.min(rows, cols)

  /** 行フルランクであるか？ */
  def isRowFullRank: Boolean = rank == rows

  /** 列フルランクであるか？ */
  def isColFullRank: Boolean = rank == cols

  /** 特異行列かどうか？ */
  def isSingular: Boolean = isSquare && !isFullRank

  /** 非特異行列かどうか？ */
  def isNonSingular: Boolean = isRowFullRank && isColFullRank

  /** 正則行列かどうか？ */
  def isRegular: Boolean = isNonSingular

  /**
   * 行列式
   *
   * @note
   *   ラプラス展開による
   */
  def determinant(using num: Numeric[N]): N = {
    require(isSquare, "The matrix must be square")

    val size = this.rows
    if size == 1 then this(0, 0)
    else {
      var det: N = num.zero
      for (col <- 0 until size) {
        val subMatrix = new Matrix[N](size - 1, size - 1)
        for {
          i <- 1 until size
          j <- 0 until size
        } {
          if j < col then subMatrix(i - 1, j) = this(i, j)
          else if j > col then subMatrix(i - 1, j - 1) = this(i, j)
        }
        val term = num.times(this(0, col), subMatrix.determinant)
        det = if col % 2 == 0 then num.plus(det, term) else num.minus(det, term)
      }
      det
    }
  }

  /**
   * @return
   *   逆行列
   */
  def inverse(using frac: Fractional[N]): Matrix[N] = {
    require(isSquare, "The matrix must be square")
    require(isNonSingular, "The matrix must be non-singular")

    val size      = this.rows
    val augmented = new Matrix[N](size, size * 2)

    // Augment the matrix with the identity matrix
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        augmented(i, j) = this(i, j)
        augmented(i, j + size) = if (i == j) frac.one else frac.zero
      }
    }

    // Apply Gauss-Jordan Elimination
    for (i <- 0 until size) {
      val pivot = augmented(i, i)
      require(pivot != frac.zero, "The matrix is singular")

      // Scale the pivot row
      for (j <- 0 until size * 2) {
        augmented(i, j) = frac.div(augmented(i, j), pivot)
      }

      // Eliminate other rows
      for (k <- 0 until size) {
        if (k != i) {
          val factor = augmented(k, i)
          for (j <- 0 until size * 2) {
            augmented(k, j) = frac.minus(augmented(k, j), frac.times(factor, augmented(i, j)))
          }
        }
      }
    }

    // Extract the inverse matrix
    val inverse = new Matrix[N](size, size)
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        inverse(i, j) = augmented(i, j + size)
      }
    }

    inverse
  }

  /**
   * 対称行列かどうか
   */
  def isSymmetric: Boolean = {
    this == this.transpose
  }

  /**
   * 正方行列かどうか
   */
  def isSquare: Boolean = rows == cols

  /**
   * 対角行列かどうか
   */
  def isDiagonal(using num: Numeric[N]): Boolean = boundary {
    if !isSquare then break(false)
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      if i != j && this(i, j) != num.zero then break(false)
    }
    true
  }

  /**
   * 恒等行列（=単位行列）かどうか
   */
  def isIdentity(using num: Numeric[N]): Boolean = boundary {
    if !isDiagonal then break(false)
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      if i == j && this(i, j) != num.one then break(false)
    }
    true
  }

  /**
   * @return
   *   行列の形状 (行数, 列数)
   */
  def shape: (Int, Int) = (rows, cols)

  def toDouble(using num: Numeric[N]): Matrix[Double] = {
    val result = Matrix[Double](rows, cols)
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      result(i, j) = num.toDouble(data(i)(j))
    }
    result
  }

  def toInt(using num: Numeric[N]): Matrix[Int] = {
    val result = Matrix[Int](rows, cols)
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      result(i, j) = num.toInt(data(i)(j))
    }
    result
  }

  def prettyString: String = {
    val rowsStrings = for (row <- data) yield row.mkString(" ")
    rowsStrings.mkString("\n")
  }
  override def toString: String = {
    val dataStr = data.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")
    s"Matrix(shape=($rows, $cols), data=$dataStr)"
  }

  override def equals(other: Any): Boolean = other match
    case that: Matrix[_] =>
      this.rows == that.rows &&
      this.cols == that.cols &&
      this.data.corresponds(that.data)(_ sameElements _)
    case _ => false

  override def hashCode(): Int = {
    val prime  = 31
    var result = 1
    result = prime * result + rows
    result = prime * result + cols
    result = prime * result + data.map(_.toSeq).toSeq.hashCode()
    result
  }
}

object Matrix {
  def apply[N: Numeric: ClassTag](rows: Int, cols: Int): Matrix[N] = new Matrix(rows, cols)

  def apply[N: Numeric: ClassTag](data: Array[Array[N]]): Matrix[N] = {
    val rows   = data.length
    val cols   = data(0).length
    val matrix = new Matrix(rows, cols)
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      matrix(i, j) = data(i)(j)
    }
    matrix
  }

  /**
   * 1 * data.length の行列を作成する = 行ベクトル
   * @param data
   *   1次元配列
   */
  def apply[N: Numeric: ClassTag](data: Array[N]): Matrix[N] = {
    val rows   = 1
    val cols   = data.length
    val matrix = new Matrix(rows, cols)
    for {
      j <- 0 until cols
    } {
      matrix(0, j) = data(j)
    }
    matrix
  }

  /**
   * 恒等行列（=単位行列）を作成する
   *
   * @example
   *   {{{
   *
   * val I = Matrix.identity[Double](3)
   * // I = [[1.0, 0.0, 0.0],
   * //      [0.0, 1.0, 0.0],
   * //      [0.0, 0.0, 1.0]]
   *   }}}
   * @param size
   *   行列のサイズ
   */
  def identity[N: Numeric: ClassTag](size: Int): Matrix[N] = {
    val matrix = new Matrix[N](size, size)
    for {
      i <- 0 until size
      j <- 0 until size
    } {
      matrix(i, j) = if i == j then summon[Numeric[N]].one else summon[Numeric[N]].zero
    }
    matrix
  }

  def I[N: Numeric: ClassTag](size: Int): Matrix[N] = identity(size)
}
