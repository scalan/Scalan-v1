package scalan.common

import collection.mutable.ArraySeq

// definitions taken from Scalan library
/**
 * A Semigroup in type S must satisfy two laws:
 * <ol>
 * <li>
 * Closure: ? a, b in S, append(a, b) is also in S. This is enforced by the type system.
 * </li>
 * <li>
 * Associativity: ? a, b and c in S, the equation append(append(a, b), c) = append(a, append(b , c)) holds.
 * </li>
 * </ol>
 * @see scalaz.Identity#?
 */
trait Semigroup[S] {
  def append(s1: S, s2: => S): S
}

trait Semigroups {
  def semigroup[S](f: (S, => S) => S): Semigroup[S] = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
  }
}

trait SemigroupLow {
}

object Semigroup extends SemigroupLow {
  import Common._

  implicit def UnitSemigroup: Semigroup[Unit] = semigroup((_, _) => ())

  implicit def StringSemigroup: Semigroup[String] = semigroup(_ + _)

  implicit def IntSemigroup: Semigroup[Int] = semigroup(_ + _)

  implicit def BooleanSemigroup: Semigroup[Boolean] = semigroup((a, b) => (a || b))

  implicit def CharSemigroup: Semigroup[Char] = semigroup((a, b) => (a + b).toChar)

  implicit def ByteSemigroup: Semigroup[Byte] = semigroup((a, b) => (a + b).toByte)

  implicit def LongSemigroup: Semigroup[Long] = semigroup((a, b) => (a + b).toLong)

  implicit def ShortSemigroup: Semigroup[Short] = semigroup((a, b) => (a + b).toShort)

  implicit def FloatSemigroup: Semigroup[Float] = semigroup((a, b) => (a + b).toFloat)

  implicit def DoubleSemigroup: Semigroup[Double] = semigroup((a, b) => (a + b).toDouble)

  implicit def BigIntegerSemigroup: Semigroup[java.math.BigInteger] = semigroup(_ add _)

  implicit def BigIntSemigroup: Semigroup[BigInt] = semigroup(_ + _)

  implicit def StreamSemigroup[A]: Semigroup[Stream[A]] = semigroup(_ append _)

  implicit def ArraySemigroup[A: Manifest]: Semigroup[Array[A]] = semigroup(Array.concat(_, _))

  implicit def ArraySeqSemigroup[A]: Semigroup[ArraySeq[A]] = semigroup(_ ++ _)

  implicit def EitherLeftSemigroup[A, B]: Semigroup[Either.LeftProjection[A, B]] = semigroup((a, b) => if (a.e.isLeft) a else b)

  implicit def EitherRightSemigroup[A, B]: Semigroup[Either.RightProjection[B, A]] = semigroup((a, b) => if (a.e.isRight) a else b)

  import java.util._

  implicit def JavaArrayListSemigroup[A]: Semigroup[ArrayList[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[ArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedListSemigroup[A]: Semigroup[LinkedList[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[LinkedList[A]]
    k addAll b
    k
  })

  implicit def JavaPriorityQueueSemigroup[A]: Semigroup[PriorityQueue[A]] = semigroup((a, b) => {
    val k = new PriorityQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaStackSemigroup[A]: Semigroup[Stack[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[Stack[A]]
    k addAll b
    k
  })

  implicit def JavaVectorSemigroup[A]: Semigroup[Vector[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[Vector[A]]
    k addAll b
    k
  })

}
