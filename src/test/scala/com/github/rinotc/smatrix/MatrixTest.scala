package com.github.rinotc.smatrix

class MatrixTest extends BaseTest {

  it("prettyString") {
    val matrix = Matrix[Int](2, 2)
    matrix.update(0, 0, 1)
    matrix.update(0, 1, 2)
    matrix.update(1, 0, 3)
    matrix.update(1, 1, 4)
    val expected = "1 2\n3 4"
    matrix.prettyString should be(expected)
  }

  describe("times") {
    it("2x2と2x2の行列の積の計算が正しい") {
      val matrix1  = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      val matrix2  = Matrix(Array(Array(5.0, 6.0), Array(7.0, 8.0)))
      val expected = Matrix(Array(Array(19.0, 22.0), Array(43.0, 50.0)))
      matrix1.times(matrix2) should be(expected)
    }

    it("2x3 と 3x5 の行列の積の計算が正しい") {
      val matrix1 = Matrix(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
      val matrix2 = Matrix(
        Array(
          Array(7.0, 8.0, 9.0, 10.0, 11.0),
          Array(12.0, 13.0, 14.0, 15.0, 16.0),
          Array(17.0, 18.0, 19.0, 20.0, 21.0)
        )
      )
      val expected = Matrix(Array(Array(82.0, 88.0, 94.0, 100.0, 106.0), Array(190.0, 205.0, 220.0, 235.0, 250.0)))
      val actual   = matrix1.times(matrix2)
      actual shouldBe expected
    }

    it("スカラー値と行列の積の計算が正しい") {
      val matrix   = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      val expected = Matrix(Array(Array(2.0, 4.0), Array(6.0, 8.0)))
      matrix.times(2.0) should be(expected)
    }
  }

  describe("plus") {
    it("2x2と2x2の行列の和の計算が正しい") {
      val matrix1  = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      val matrix2  = Matrix(Array(Array(5.0, 6.0), Array(7.0, 8.0)))
      val expected = Matrix(Array(Array(6.0, 8.0), Array(10.0, 12.0)))
      matrix1.plus(matrix2) should be(expected)
    }

    it("2x3 と 2x3 の行列の和の計算が正しい") {
      val matrix1  = Matrix(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
      val matrix2  = Matrix(Array(Array(7.0, 8.0, 9.0), Array(10.0, 11.0, 12.0)))
      val expected = Matrix(Array(Array(8.0, 10.0, 12.0), Array(14.0, 16.0, 18.0)))
      matrix1.plus(matrix2) should be(expected)
    }
  }

  describe("trace") {
    it("2x2の行列のトレースの計算が正しい") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      matrix.trace should be(5.0)
    }
  }

  describe("rank") {
    it("正方行列でフルランクの場合は次元と等しい") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      matrix.rank should be(2)
    }

    it("正方行列でランクが行列の次元よりも小さい場合") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(2.0, 4.0)))
      matrix.rank should be(1)
    }

    it("非正方行列でランクが行数と等しい場合") {
      val matrix = Matrix(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
      matrix.rank should be(2)
    }

    it("非正方行列でランクが行数よりも小さい場合") {
      val matrix = Matrix(Array(Array(1.0, 2.0, 3.0), Array(2.0, 4.0, 6.0)))
      matrix.rank should be(1)
    }
  }

  describe("transpose") {
    it("2x3の行列を転置したら3x2の行列になる") {
      val matrix   = Matrix(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
      val expected = Matrix(Array(Array(1.0, 4.0), Array(2.0, 5.0), Array(3.0, 6.0)))
      matrix.transpose should be(expected)
    }
  }

  describe("等価性のテスト") {
    it("要素が一つでも異なれば、false") {
      val matrix1 = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      val matrix2 = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.1)))
      matrix1.equals(matrix2) should be(false)
    }

    it("要素が全て一致していればtrue") {
      val matrix1 = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      val matrix2 = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      matrix1.equals(matrix2) should be(true)
    }
  }

  describe("isSymmetric") {
    it("対称行列の場合はtrue") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(2.0, 3.0)))
      matrix.isSymmetric shouldBe true
    }

    it("対称行列でない場合はfalse") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      matrix.isSymmetric shouldBe false
    }
  }

  describe("isSquare") {
    it("正方行列の場合はtrue") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      matrix.isSquare shouldBe true
    }

    it("正方行列でない場合はfalse") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0), Array(5.0, 6.0)))
      matrix.isSquare shouldBe false
    }
  }

  describe("isDiagonal") {
    it("対角行列の場合はtrue") {
      val matrix = Matrix(Array(Array(1.0, 0.0), Array(0.0, 2.0)))
      matrix.isDiagonal shouldBe true
    }

    it("対角行列でない場合はfalse") {
      val matrix = Matrix(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
      matrix.isDiagonal shouldBe false
    }
  }

  describe("isIdentity") {
    it("単位行列の場合はtrue") {
      val matrix = Matrix(Array(Array(1.0, 0.0), Array(0.0, 1.0)))
      matrix.isIdentity shouldBe true
    }

    it("単位行列でない場合はfalse") {
      val matrix = Matrix(Array(Array(1.0, 0.0), Array(0.0, 4.0)))
      matrix.isIdentity shouldBe false
    }
  }

  describe("companion") {
    describe("apply") {
      it("1次元配列によるapply") {
        val array  = Array(1, 2, 3, 4)
        val matrix = Matrix(array)
        matrix.rows shouldBe 1
        matrix.cols shouldBe 4
        matrix(0, 0) shouldBe 1
        matrix(0, 1) shouldBe 2
        matrix(0, 2) shouldBe 3
        matrix(0, 3) shouldBe 4
      }

      it("2次元配列によるapply") {
        val array  = Array(Array(1, 2), Array(3, 4))
        val matrix = Matrix(array)
        matrix.rows shouldBe 2
        matrix.cols shouldBe 2
        matrix(0, 0) shouldBe 1
        matrix(0, 1) shouldBe 2
        matrix(1, 0) shouldBe 3
        matrix(1, 1) shouldBe 4
      }
    }

    describe("identity") {
      it("単位行列の生成") {
        val matrix = Matrix.identity[Double](3)
        matrix shouldBe Matrix(Array(Array(1.0, 0.0, 0.0), Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 1.0)))
      }
    }
  }
}
