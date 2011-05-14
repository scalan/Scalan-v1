package scalan.samples

import scalan.dsl._

trait DslSamples extends Scalan {
  def odd(x: Int) = x % 2 != 0

  def vectorOfSquares(len: Int):PA[Int] = for (x <- fromArray(0 to len) if odd(x)) yield (x * x)

  type VectorElem = (Int,Float)
  type SparseVector = PA[VectorElem]
  type Vector = PA[Float]
  type Matrix = PA[SparseVector]

  def dotProduct(v1: Vector, v2: Vector): Float = sum(for ((f1,f2) <- v1 zip v2) yield f1 * f2)
  def dotProduct2(v1: Vector, v2: Vector): Float = (v1, v2).zippedPA.map{_ * _}.sum

  def sparseVectorMul(sv: SparseVector, v: Vector) = sum(for ((i,value) <- sv) yield v(i) * value)

  def matrixVectorMul(mat: Matrix, vec: Vector) = for (row <- mat) yield sparseVectorMul(row, vec)

  def qsort(xs: PA[Int]): PA[Int] = {
    val len = xs.length
    if (len <= 1) xs
    else {
      val m = xs(len / 2)
      val smaller = for (x <- xs if x < m) yield x     // can use for-comprehesions
      val greater = xs filter(x => x > m)              // or methods directly
      val equal = xs filter(_ == m)                    // or even shorter
      val sg = fromArray(Array(smaller, greater))
      val sorted = for (sub <- sg) yield qsort(sub)
      sorted(0) ++ equal ++ sorted(1)
    }
  }
}

trait StdSamples {
  type VectorElem = (Int,Float)
  type SparseVector = Array[VectorElem]
  type Vector = Array[Float]
  type Matrix = Array[SparseVector]

  def dotProduct(v1: Vector, v2: Vector): Float = (for ((f1,f2) <- v1 zip v2) yield f1 * f2).sum

  def dotProduct2(v1: Vector, v2: Vector): Float = (v1, v2).zipped.map{ _ * _ }.sum
}

