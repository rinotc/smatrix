package com.github.rinotc.smatrix.immutable

import scala.annotation.targetName
import scala.util.boundary
import scala.util.boundary.break

final class Matrix[N: Numeric] private (private val data: Vector[Vector[N]]) {

  import Matrix.*

  requirements(data).left.foreach(m => throw new IllegalArgumentException(m))

  /**
   * 行数（> 0）
   */
  val rows: Int = data.length

  /**
   * 列数（> 0）
   */
  val cols: Int = data.head.length

  def apply(row: Int): Vector[N] = data(row)

  def apply(row: Int, col: Int): N = data(row)(col)

  def updated(row: Int, col: Int, value: N): Matrix[N] = {
    val updatedRow = data(row).updated(col, value)
    new Matrix(data.updated(row, updatedRow))
  }

  def updated(row: Int, value: Vector[N]): Matrix[N] = {
    new Matrix(data.updated(row, value))
  }

  def shape: (Int, Int) = (rows, cols)

  @targetName("plusSymbol")
  def +(that: Matrix[N]): Matrix[N] = plus(that)

  @targetName("minusSymbol")
  def -(that: Matrix[N]): Matrix[N] = minus(that)

  @targetName("timesSymbol")
  def *(that: Matrix[N]): Matrix[N] = times(that)

  @targetName("timesSymbol")
  def *(that: N): Matrix[N] = times(that)

  /**
   * 行列の加算
   *
   * A + B = (a_ij) + (b_ij) = (a_ij + b_ij)
   *
   * @param that
   *   加算する行列
   */
  def plus(that: Matrix[N]): Matrix[N] = {
    require(this.shape == that.shape, "shape mismatch")
    val num = summon[Numeric[N]]
    new Matrix(data.zip(that.data).map { case (row1, row2) =>
      row1.zip(row2).map { case (x, y) => num.plus(x, y) }
    })
  }

  /**
   * 行列の減算
   *
   * A - B = (a_ij) - (b_ij) = (a_ij - b_ij)
   *
   * @param that
   *   減算する行列
   */
  def minus(that: Matrix[N]): Matrix[N] = {
    val num = summon[Numeric[N]]
    this + (that * num.negate(num.one))
  }

  /**
   * 行列の乗法
   *
   * A * B = (a_ij) * (b_ij) = (a_i1 * b_1j + a_i2 * b_2j + ... + a_in * b_nj)
   *
   * @param that
   *   乗法する行列 (this.cols == that.rows である必要がある)
   */
  def times(that: Matrix[N]): Matrix[N] = {
    val num = summon[Numeric[N]]
    require(this.cols == that.rows, "shape mismatch")
    new Matrix(Vector.tabulate(this.rows) { i =>
      Vector.tabulate(that.cols) { j =>
        (0 until this.cols).foldLeft(num.zero) { (acc, k) =>
          num.plus(acc, num.times(this(i, k), that(k, j)))
        }
      }
    })
  }

  /**
   * スカラー乗法
   *
   * c * A = c * (a_ij) = (c * a_ij)
   *
   * @param that
   *   スカラー値
   */
  def times(that: N): Matrix[N] = {
    val num = summon[Numeric[N]]
    new Matrix(data.map { row =>
      row.map { x => num.times(x, that) }
    })
  }

  /**
   * 正方行列かどうか
   */
  def isSquare: Boolean = rows == cols

  /**
   * 行列の転置
   *
   * @return
   *   転置行列
   */
  lazy val transpose: Matrix[N] = {
    new Matrix(Vector.tabulate(cols) { j =>
      Vector.tabulate(rows) { i =>
        data(i)(j)
      }
    })
  }

  /**
   * 行列のトレース（=対角成分の和）
   *
   * `tr(A) = a_11 + a_22 + ... + a_nn `
   *
   * @return
   *   トレース
   */
  lazy val trace: N = {
    require(
      isSquare,
      "trace is only defined for square matrices"
    )
    val num = summon[Numeric[N]]
    (0 until rows).foldLeft(num.zero) { (acc, i) =>
      num.plus(acc, data(i)(i))
    }
  }

  /** 行列式 */
  lazy val determinant: N = {
    val num = summon[Numeric[N]]
    require(
      isSquare,
      "determinant is only defined for square matrices"
    )

    def det(matrix: Matrix[N]): N = {
      if matrix.rows == 1 then matrix(0, 0)
      else if matrix.rows == 2 then
        num.minus(
          num.times(matrix(0, 0), matrix(1, 1)),
          num.times(matrix(0, 1), matrix(1, 0))
        )
      else
        (0 until matrix.cols).foldLeft(num.zero) { (acc, j) =>
          num.plus(
            acc,
            num.times(
              num.times(matrix(0, j), num.fromInt(math.pow(-1, j).toInt)),
              det(matrix.minor(0, j))
            )
          )
        }
    }
    det(this)
  }

  /**
   * 指定した行と列を除いた行列
   */
  def minor(rowToRemove: Int, colToRemove: Int): Matrix[N] = {
    val newData = data.zipWithIndex
      .filter { case (_, index) => index != rowToRemove }
      .map { case (row, _) => row.zipWithIndex.filter { case (_, index) => index != colToRemove }.map(_._1) }
    new Matrix(newData)
  }

  /**
   * 逆行列
   */
  def inverse(using frac: Fractional[N]): Matrix[N] = {
    import frac.{div, fromInt, one, times}
    require(isSquare, "inverse is only defined for square matrices")
    require(isRegular, "inverse is only defined for regular matrices")

    val adjugate = new Matrix(Vector.tabulate(rows) { i =>
      Vector.tabulate(cols) { j =>
        val sign        = fromInt(math.pow(-1, i + j).toInt)
        val minorMatrix = minor(i, j)
        frac.times(sign, minorMatrix.determinant)
      }
    }).transpose

    adjugate * div(one, determinant)
  }

  /** 行列のランク */
  def rank: Int = {
    var m    = this.toDouble // 元の行列を変更しないためにコピーを作成
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
        m = m.updated(pivotRow, m(rank))
        m = m.updated(rank, tmp)

        val pivot = m(rank, col)
        // ピボット行をピボットで割る
        for (j <- 0 until m.cols) {
          m = m.updated(rank, j, m(rank, j) / pivot)
        }

        // ピボット列の他のすべての要素を0にする
        for (i <- 0 until m.rows) {
          if (i != rank) {
            val factor = m(i, col)
            for (j <- 0 until m.cols) {
              m = m.updated(i, j, m(i, j) - factor * m(rank, j))
            }
          }
        }

        rank += 1
      }
    }
    rank
  }

  /** 正則行列かどうか */
  def isRegular: Boolean = determinant != summon[Numeric[N]].zero

  /** 対称行列かどうか */
  def isSymmetric: Boolean = this == transpose

  /** 対角行列かどうか */
  def isDiagonal: Boolean = {
    val num = summon[Numeric[N]]
    (0 until rows).forall { i =>
      (0 until cols).forall { j =>
        if i == j then data(i)(j) == num.zero else data(i)(j) != num.zero
      }
    }
  }

  /** 単位行列かどうか */
  def isIdentity: Boolean = {
    this == one(rows)
  }

  def toDouble: Matrix[Double] = {
    val num = summon[Numeric[N]]
    new Matrix(data.map(_.map(num.toDouble)))
  }

  def toFloat: Matrix[Float] = {
    val num = summon[Numeric[N]]
    new Matrix(data.map(_.map(num.toFloat)))
  }

  def toInt: Matrix[Int] = {
    val num = summon[Numeric[N]]
    new Matrix(data.map(_.map(num.toInt)))
  }

  def toLong: Matrix[Long] = {
    val num = summon[Numeric[N]]
    new Matrix(data.map(_.map(num.toLong)))
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
      this.data == that.data
    case _ => false

  override def hashCode(): Int = {
    val prime  = 31
    var result = 1
    result = prime * result + rows
    result = prime * result + cols
    result = prime * result + data.hashCode()
    result
  }
}

object Matrix {

  def apply[N: Numeric](data: Seq[Seq[N]]): Matrix[N] = new Matrix(data.toVector.map(_.toVector))

  def apply[N: Numeric](rows: Int, cols: Int): Matrix[N] = {
    val data = Vector.fill(rows)(Vector.fill(cols)(summon[Numeric[N]].zero))
    new Matrix(data)
  }

  def vec[N: Numeric](data: Seq[N]): Matrix[N] = new Matrix(Vector(data.toVector))

  private def requirements[N: Numeric](data: Seq[Seq[N]]): Either[String, Unit] = {
    for {
      _ <- Either.cond(data.nonEmpty, (), "rows must not be empty")           // 1行以上
      _ <- Either.cond(data.forall(_.nonEmpty), (), "cols must not be empty") // 1列以上
      _ <- {
        // 全ての行が同じ列数であること
        val cols = data.head.length
        Either.cond(data.forall(_.length == cols), (), "All rows must have the same size")
      }
    } yield ()
  }

  /**
   * 単位行列
   */
  def one[N: Numeric](size: Int): Matrix[N] = {
    val num = summon[Numeric[N]]
    val data = Vector.tabulate(size) { i =>
      Vector.tabulate(size) { j =>
        if i == j then num.one else num.zero
      }
    }
    new Matrix(data)
  }

  def I[N: Numeric](size: Int): Matrix[N] = one(size)

  def identity[N: Numeric](size: Int): Matrix[N] = one(size)

  def zero[N: Numeric](rows: Int, cols: Int): Matrix[N] = {
    val num  = summon[Numeric[N]]
    val data = Vector.fill(rows)(Vector.fill(cols)(num.zero))
    new Matrix(data)
  }

  def zero[N: Numeric](size: Int): Matrix[N] = zero(size, size)
}
