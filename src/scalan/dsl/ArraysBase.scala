package scalan.dsl
import scala.text._
import Document._
import scalan.util.ArrayFormatting._
import scalan.common._
import Common._

trait ArraysBase  {
  type PA[T] <: PArray[T]              // parallel array of T
  type Elem[T] <: Element[T, PA]      // type class of array element
  type |[A,B] = Either[A,B]
  type IntRep = Int
  type BoolRep = Boolean
  type UnitRep = Unit
  type ByteRep = Byte
  type ShortRep = Short
  type CharRep = Char
  type LongRep = Long
  type FloatRep = Float
  type DoubleRep = Double

  trait Element[a, C[_]] {
    def replicate(count: IntRep, v: a): C[a]                     // (3,v) -> (v,v,v)
    def replicateSeg(count: IntRep, v: C[a]): C[a]               // (3,(a,b)) -> (a,b,a,b,a,b)
    def tabulate(len: IntRep)(f:IntRep => a): C[a]
    def tabulateSeg(len: IntRep)(f:IntRep => C[a]): C[a]
    def defaultOf(implicit z: Zero[a]) = mzero[a]
    def empty: C[a]
    def singleton(v:a) = replicate(1,v)
    def fromArray(arr: Array[a]) = tabulate(arr.length)(i => arr(i))
    def manifest: ClassManifest[a]
  }
  //implicit def elemToManifest[A](implicit ea: Elem[A]): ClassManifest[A] = ea.manifest

  trait PArray[T] { self: PA[T] =>
    def length: IntRep
    def index(i: IntRep): T
    def toArray: Array[T]
    def map[B:Elem](f: T => B): PA[B]
    def flatMap[B:Elem](f:T=>PA[B]): PA[B]
    def filter(f: T=>BoolRep): PA[T] = { val flags = for (e <- this) yield f(e); pack(flags) }
    def withFilter(f: T=>BoolRep): PA[T]
    def apply(i: IntRep) = index(i)

    def zip[B](b: PA[B])(implicit eb:Elem[B], etb: Elem[(T,B)]): PA[(T, B)]

    def zipWith[B,C]
          (f: T=>B=>C)(that: PA[B])
          (implicit eb:Elem[B], ec:Elem[C], etb: Elem[(T,B)]): PA[C] =
      for ((a,b) <- this.zip(that)) yield f(a)(b)

    def backPermute(idxs: PA[IntRep]): PA[T]
    def permute(idxs: PA[IntRep]): PA[T]

    def slice(start:IntRep, len:IntRep): PA[T]
    def scan(implicit s:Semigroup[T]): PA[T]                                                 // (1,2,3,4,5) -> (0,1,3,6,10)
    def pack(flags: PA[BoolRep]): PA[T] = { val (res,_) = this.flagSplit(flags); res }
    def sum(implicit m: Monoid[T]): T = {
      val len = length
      if (len == 0) return mzero[T]
      val last = length - 1
      m.append(scan(m)(last), this(last))
    }
    /**
     * length(this) == length(ifFalse) == length(flags)
     */
    def flagCombine(ifFalse: PA[T], fs: PA[BoolRep]): PA[T]

    def flagMerge  (ifFalse: PA[T], flags: PA[BoolRep]): PA[T]  // length(this) + length(ifFalse) == length(flags)
    def flagSplit  (flags: PA[BoolRep]): (PA[T], PA[T])         // length(this) == length(flags) == (length(A) + length(B))

    def ++(that: PA[T])(implicit epa:Elem[PA[T]]): PA[T]
    def toDoc: Document = text(toString)

    implicit val elem: Elem[T]
    implicit val elem2: Elem[(T,T)]
  }

  abstract class StdArray[A](val arr: Array[A]) extends PArray[A] { self: PA[A] =>
  }

  abstract class PairArray[A, B](val a: PA[A], val b: PA[B]) extends PArray[(A,B)] {
    self: PA[(A,B)] =>
    def length = a.length
    def index(i: IntRep) = (a(i), b(i))
    override def toDoc = group("PairArray(" :: nest(2,a.toDoc) :: ", " :/: nest(2,b.toDoc) :/: ")" :: ED)
  }

  abstract class SumArray[A, B](val flags: PA[BoolRep], val a: PA[A], val b: PA[B]) extends PArray[(A|B)] {
    self: PA[(A|B)] =>
    def length = flags.length
    override def toDoc = group("SumArray(" :: nest(2,flags.toDoc) :: ", " :/: nest(2,a.toDoc) :: ", " :/: nest(2,b.toDoc) :: ")" :: ED)
  }

  abstract class NestedArray[A](val arr: PA[A], val segments: PA[(IntRep,IntRep)]) extends PArray[PA[A]] {
    self: PA[PA[A]] =>
    def length = segments.length
    def index(i: IntRep) = { val (p,l) = segments(i); arr.slice(p,l) }
    override def toDoc = group("NestedArray(" :: nest(2,arr.toDoc) :: ", " :/: nest(2,segments.toDoc) :: ")" :: ED)
  }

  def unzip[A: Elem, B: Elem](a: PA[(A, B)]): (PA[A], PA[B]) = a match {
    case pair: PairArray[A,B] => (pair.a, pair.b)
    case _ => error("expected PairArray but was " + a)
  }

  def concat[A](a: PA[PA[A]]): PA[A] = a match {
    case nested: NestedArray[A] => nested.arr
    case _ => error("expected NestedArray but was " + a)
  }

  def unconcat[A,B](a: PA[PA[A]])(b:PA[B])(implicit ea: Elem[B], epa: Elem[PA[B]]): PA[PA[B]]

  def emptyArrayOf[T:Elem] = element[T].empty

  def element[A](implicit ea:Elem[A]): Elem[A] = ea

  trait TuplePA[A,B] extends PimpedType[(PA[A],PA[B])] {
    def zippedPA(implicit eb:Elem[B], eab: Elem[(A,B)]): Zipped[A, B] = new Zipped[A,B](value._1, value._2)

    class Zipped[A,B](a: PA[A], b: PA[B])(implicit eb:Elem[B], eab: Elem[(A,B)]) {
      def map[C:Elem](f: (A, B) => C): PA[C] = a.zip(b).map {case (l,r) => f(l, r) }
    }
  }
  implicit def toTuplePA[A,B](p: (PA[A],PA[B])) = new TuplePA[A,B] { val value = p  }


  implicit val boolElement:  Elem[BoolRep]
  implicit val byteElement:  Elem[ByteRep]
  implicit val shortElement: Elem[ShortRep]
  implicit val charElement:  Elem[CharRep]
  implicit val intElement:   Elem[IntRep]
  implicit val longElement:  Elem[LongRep]
  implicit val floatElement: Elem[FloatRep]
  implicit val doubleElement:Elem[DoubleRep]
  implicit val unitElement:  Elem[UnitRep]
  implicit val stringElement:  Elem[String]

  implicit def pairElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A,B)]
  implicit def sumElement [A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A|B)]

  implicit def arrayElement[A](implicit a: Elem[A], z: Zero[A]): Elem[PA[A]]
  implicit def paZero[T:Elem]: Zero[PA[T]] = zero(element[T].empty)
  implicit def sumZero[A,B](implicit ea:Elem[A], z:Zero[A]): Zero[(A|B)] = zero(Left(element[A].defaultOf))
  implicit def pairZero[A,B](implicit ea:Elem[A], za:Zero[A], eb:Elem[B], zb:Zero[B]): Zero[(A,B)] = zero((element[A].defaultOf, element[B].defaultOf))

  implicit def intToRep(i: Int): IntRep     // required on usage side to inject base types in representation space
}
