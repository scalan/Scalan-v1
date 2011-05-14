package scalan.sequential

import scalan.dsl._

trait SeqArrays extends Arrays with SeqImplementation {
  def length[T: Elem](a: PA[T]): Int = a.length
  def fromArray[T: Elem](x: Array[T]): PA[T] = element[T].fromArray(x)
  def index[T: Elem](a: PA[T], i: Int): T = a.index(i)
  def singleton[T:Elem](v: T): PA[T] = element[T].singleton(v)
  def replicate[T:Elem](count: Int, v: T): PA[T] = element[T].replicate(count, v)
  def replicateSeg[T:Elem](count: Int, vs: PA[T]): PA[T] = element[T].replicateSeg(count, vs)
  def tabulate[T:Elem](len:Int)(f:Int => T): PA[T] = element[T].tabulate(len)(f)
  def tabulateSeg[T:Elem](len:Int)(f:Int => PA[T]): PA[T] = element[T].tabulateSeg(len)(f)
  def map[A,B:Elem](f: A => B)(a: PA[A]): PA[B] = a.map(f)
  def flatMap[A,B:Elem](f:A=>PA[B])(a: PA[A]): PA[B] = a.flatMap(f)
  def filter[A](f: A=>Boolean)(a: PA[A]): PA[A] = a.filter(f)
  def zip[A,B](a: PA[A], b: PA[B])(implicit ea:Elem[A], eb:Elem[B], eab:Elem[(A,B)]): PA[(A, B)] = a.zip(b)
  def permute[T:Elem](a: PA[T], idxs: PA[Int]): PA[T] = a.permute(idxs)
  def backPermute[T:Elem](a: PA[T], idxs: PA[Int]): PA[T] = a.backPermute(idxs)
  def slice[T](a: PA[T], start: Int, len: Int): PA[T] = a.slice(start, len)

  def unconcat[A, B](shapeArr: PA[PA[A]])(arr: PA[B])(implicit ea: Elem[A], eb: Elem[B], epb: Elem[PA[B]]): PA[PA[B]] = shapeArr match {
    case nested: NestedArray[A] => SeqNestedArray(arr, nested.segments)
    case _ => error("expected NestedArray but was " + shapeArr)
  }
  def flagCombine[A](flags: PA[Boolean], ifTrue: PA[A], ifFalse: PA[A]) = ifTrue flagCombine (ifFalse, flags)
  def flagMerge  [A](flags: PA[Boolean], ifTrue: PA[A], ifFalse: PA[A]) = ifTrue flagMerge (ifFalse, flags)
  def flagSplit  [A](flags: PA[Boolean], a: PA[A]) = a flagSplit flags

}
//trait BasisArrayOperationImpl[T] extends BasisArrayOperations[T] with Arrays {
//  def infix_-(a: PA[T], b: PA[T]) = {
//    map(case (a,b) => a - b)(zip(a,b))
//  }
//}

