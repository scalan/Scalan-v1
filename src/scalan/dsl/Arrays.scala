package scalan.dsl

import scalan.common.{Monoid, Semigroup}

trait Arrays extends ArraysBase {
  def fromArray[T: Elem](x: Array[T]): PA[T]
  def length[T: Elem](a: PA[T]): IntRep
  def index [T: Elem](a: PA[T], i: IntRep): T
  def singleton[T:Elem](v: T): PA[T]
  def replicate[T:Elem](count: IntRep, v: T): PA[T]
  def replicateSeg[T:Elem](count: IntRep, vs: PA[T]): PA[T]
  def tabulate[T:Elem](len:IntRep)(f:IntRep => T): PA[T]
  def tabulateSeg[T:Elem](len:IntRep)(f:IntRep => PA[T]): PA[T]

  def map[A,B:Elem](f: A => B)(a: PA[A]): PA[B]
  def zip[A,B](a: PA[A], b: PA[B])(implicit ea: Elem[A], eb: Elem[B], eab: Elem[(A, B)]): PA[(A, B)]

  // place elements to specified indexes
  def permute[T:Elem](a: PA[T], idxs: PA[IntRep]): PA[T]

  // retrieve elements from specified indexes
  def backPermute[T:Elem](a: PA[T], idxs: PA[IntRep]): PA[T]

  def slice[T](a: PA[T], start: IntRep, len: IntRep): PA[T]
  def unconcat[A, B](shapeArr: PA[PA[A]])(arr: PA[B])(implicit ea: Elem[A], eb: Elem[B], epb: Elem[PA[B]]): PA[PA[B]]

  def filter[A](f: A=>BoolRep)(a: PA[A]): PA[A]
  def flatMap[A,B:Elem](f:A=>PA[B])(a: PA[A]): PA[B]

  def flagCombine[A](flags: PA[BoolRep], ifTrue: PA[A], ifFalse: PA[A]): PA[A]  // length(ifTrue) == length(ifFalse) == length(flags)
  def flagMerge  [A](flags: PA[BoolRep], ifTrue: PA[A], ifFalse: PA[A]): PA[A]  // length(ifTrue) + length(ifFalse) == length(flags)
  def flagSplit  [A](flags: PA[BoolRep], a: PA[A]): (PA[A], PA[A])              // length(ifTrue) + length(ifFalse) == length(flags)


  ////  def transpose[A](a: PA[PA[A]]): PA[PA[A]]
  //def zipWith[A,B,C](f: A=>B=>C)(as: PA[A], bs: PA[B])(implicit ea: Elem[A], eb: Elem[B], eab: Elem[(A, B)], m: ClassManifest[(A, B)]): PA[C]
  def pack[A](a: PA[A], flags: PA[BoolRep]): PA[A] = a.pack(flags)           // remove all corresponding to false preserving order
  ////
  ////  def expand[A,B](a: PA[PA[A]])(arr: PA[B]): PA[B]
  ////
  ////  def reduce[A: Semigroup](a: PA[A]): A
  def scan[A:Semigroup](a: PA[A]): PA[A] = a.scan
  def sum [A](a:PA[A])(implicit m: Monoid[A]): A = a.sum(m)


  implicit def rangeToArr(r: Range) = r.toArray

  //  abstract class PAE[T](implicit w: elemA.Elem => T) extends PimpedType[PA[T]] {
  //    def apply(idx: Int): T = index(value, idx)
  ////    def ++(a:PA[T]): PA[T] = infix_++(value, a)
  //  }
  //  implicit def pimpPArray[T](a: PA[T])(implicit w: elemA.Elem => T) = new PAE[T] { val value = a }
}

trait BasisArrayOperations[T] extends ArraysBase {
  def infix_-(a: PA[T], b: PA[T]): PA[T]

  class BasisOps(lhs: PA[T]) {
    def -(rhs: PA[T]) = infix_-(lhs, rhs)
  }

  implicit def mkBasisOps(lhs: PA[T]): BasisOps = new BasisOps(lhs)
}
