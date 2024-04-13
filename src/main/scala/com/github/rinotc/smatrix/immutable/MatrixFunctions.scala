package com.github.rinotc.smatrix.immutable

/** 行列式 */
def det[N](matrix: Matrix[N])(using num: Numeric[N]): N = matrix.determinant

/** トレース */
def tr[N](matrix: Matrix[N])(using num: Numeric[N]): N = matrix.trace

/** 転置 */
def T[N](matrix: Matrix[N]): Matrix[N] = matrix.transpose

