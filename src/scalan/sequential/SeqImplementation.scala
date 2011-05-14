package scalan.sequential

import scala.reflect.ClassManifest
import scalan.common._
import Common._
import scala.text._
import Document._
import scalan.util.ArrayFormatting._
import scalan.util.Utils
import Utils._
import scalan.dsl._

trait SequentialBase {

}

trait SeqImplementation extends ArraysBase {
  type PA[T] = PArray[T]
  type Elem[T] = Element[T, PA]
//  type IntRep = Int
//  type BoolRep = Boolean
//  type UnitRep = Unit
//  type ByteRep = Byte
//  type ShortRep = Short
//  type CharRep = Char
//  type LongRep = Long
//  type FloatRep = Float
//  type DoubleRep = Double

  def intToRep(i: Int): IntRep = i

  override implicit val boolElement:  Elem[Boolean] = new StdElement[Boolean]
  override implicit val byteElement:  Elem[Byte] = new StdElement[Byte]
  override implicit val shortElement: Elem[Short] = new StdElement[Short]
  override implicit val charElement:  Elem[Char] = new StdElement[Char]
  override implicit val intElement:   Elem[Int] = new StdElement[Int]
  override implicit val longElement:  Elem[Long] = new StdElement[Long]
  override implicit val floatElement: Elem[Float] = new StdElement[Float]
  override implicit val doubleElement:Elem[Double] = new StdElement[Double]
  override implicit val unitElement:  Elem[Unit] = new UnitElement
  override implicit val stringElement:Elem[String] = new StdElement[String]

  abstract class UnitArray(val len: Int) extends PArray[UnitRep] {
    self: PA[UnitRep] =>
    def length = len
    def index(i: IntRep) = {}
    override def toDoc = group("UnitArray(" + len +  ")" :: ED)
  }

  def mkStd[A](len:Int)(f:Int=>A)(implicit s: Semigroup[A],m:Manifest[A],ea:Elem[A], z:Zero[A]) = SeqStdArray(Array.tabulate(len)(f))

  class StdElement[T](implicit s: Semigroup[T], z:Zero[T], m:ClassManifest[T]) extends Element[T, PArray] {
    implicit val elemT = this
    def manifest: ClassManifest[T] = m
    def replicate(count: Int, v: T) = SeqStdArray(Array.fill(count)(v))
    def replicateSeg(count: Int, v: PA[T]) = {
      val vlen = v.length
      SeqStdArray(Array.tabulate(count * vlen)(i => v(i % vlen)))
    }
    def tabulate(len: Int)(f:Int => T) = SeqStdArray(Array.tabulate(len)(f))
    def tabulateSeg(len: Int)(f:Int => PA[T]) = SeqStdArray(Array.concat(Array.tabulate(len)(i => f(i).toArray): _*))
    def empty = replicate(0, mzero[T])
  }

  class UnitElement(implicit m:ClassManifest[Unit]) extends Element[Unit, PArray] {
    def manifest: ClassManifest[Unit] = m
    def replicate(count: Int, v: Unit) = new SeqUnitArray(count)
    def replicateSeg(count: Int, v: PA[Unit]) = new SeqUnitArray(count * v.length)
    def tabulate(len: Int)(f:Int => Unit) = SeqUnitArray(len)
    def tabulateSeg(len: Int)(f:Int => PA[Unit]) = { val lens = Array.tabulate(len)(i => f(i).length); SeqUnitArray(lens.sum) }
    def empty = replicate(0, ())
  }

  override implicit def pairElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A,B)] =
    new Element[(A,B), PArray] {
      implicit val elemAB = this
      val m: ClassManifest[(A,B)] = ClassManifest.classType(classOf[(A,B)], ea.manifest, eb.manifest)
      def manifest: ClassManifest[(A,B)] = m
      def replicate(count: Int, v: (A,B)) = new SeqPairArray(ea.replicate(count, v._1), eb.replicate(count, v._2))
      def replicateSeg(count: Int, v: PA[(A,B)]) = {
        val (a,b) = unzip(v)
        SeqPairArray(ea.replicateSeg(count, a), eb.replicateSeg(count, b))
      }
      def tabulate(len: Int)(f:Int => (A,B)) = {
        val temp = Array.tabulate(len)(f)
        val a1 = ea.tabulate(len)(i => temp(i)._1)
        val a2 = eb.tabulate(len)(i => temp(i)._2)
        SeqPairArray(a1, a2)
      }
      def tabulateSeg(len: Int)(f:Int => PA[(A,B)]) = {
        val temp = Array.tabulate(len)(f)
        val a1 = ea.tabulateSeg(len)(i => unzip(temp(i))._1)
        val a2 = eb.tabulateSeg(len)(i => unzip(temp(i))._2)
        SeqPairArray(a1, a2)
      }
      def empty = SeqPairArray(ea.empty, eb.empty)
    }

  override implicit def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A|B)] =
    new Element[(A|B), PArray]
    {
      implicit val elemAB = this
      val boolElem = element[Boolean]
      val m: ClassManifest[(A|B)] = ClassManifest.classType(classOf[(A|B)], ea.manifest, eb.manifest)

      def manifest: ClassManifest[(A|B)] = m

      def replicate(count: Int, v: (A|B)) = {
        v.fold(
          a => {
            val as = ea.replicate(count, a)
            SeqSumArray(boolElem.replicate(count, false), as, eb.empty)
          },
          b => {
            val bs = eb.replicate(count, b)
            SeqSumArray(boolElem.replicate(count, true), ea.empty, bs)
          }
        )
      }

      def replicateSeg(count: Int, v: PA[(A|B)]) = {
        val arr = v.asInstanceOf[SeqSumArray[A,B]]
        SeqSumArray(
          boolElem.replicateSeg(count, arr.flags),
          ea.replicateSeg(count, arr.a),
          eb.replicateSeg(count, arr.b))
      }

      def tabulate(len: Int)(f:Int => (A|B)) = {
        val temp = Array.tabulate(len)(f)
        fromArray(temp)
      }
      def tabulateSeg(len: Int)(f:Int => PA[(A|B)]) = {
        val temp = Array.concat(Array.tabulate(len)(i => f(i).toArray): _*)
        fromArray(temp)
      }
      override def fromArray(arr: Array[(A|B)]) = {  //TODO implement using flagCombine
        val len = arr.length
        val flags = mkStd(len)(i => arr(i).isRight)
        val left = for (l <- arr; if l.isLeft) yield l.left.get
        val right = for (r <- arr; if r.isRight) yield r.right.get

        SeqSumArray(flags, ea.tabulate(left.length)(i => left(i)), eb.tabulate(right.length)(i => right(i)))
      }

      def empty = SeqSumArray(boolElem.empty, ea.empty, eb.empty)
    }

  override implicit def arrayElement[A](implicit ea: Elem[A], z: Zero[A]): Elem[PA[A]] =
    new Element[PA[A], PA] {
      implicit val elemPA = this
      val segElem = element[(Int,Int)]
      val intElem = element[Int]
      implicit val m:ClassManifest[PA[A]] = ClassManifest.classType(classOf[PA[A]], ea.manifest)
      def manifest: ClassManifest[PA[A]] = m

      def replicate(count: Int, v: PA[A]): PA[PA[A]] = {
        if (count == 0) return empty
        val segLen = v.length
        if (v.length > 0) {
          val firstItem = v(0)
          val arr = ea.replicateSeg(count, v)
          val lens = intElem.replicate(count, segLen)
          val positions = lens.scan
          new SeqNestedArray(arr, SeqPairArray(positions, lens))
        }
        else
        {
          val arr = ea.replicate(0, ea.defaultOf)
          val segments = segElem.replicate(count, (0,0))
          new SeqNestedArray(arr, segments)
        }
      }

      def replicateSeg(count: Int, v: PA[PA[A]]): PA[PA[A]] = {
        if (count == 0) return empty
        val segLen = v.length
        if (segLen == 0) {
          val arr = ea.replicate(0, ea.defaultOf)
          val segments = segElem.replicate(count, (0, 0))
          SeqNestedArray(arr, segments)
        }
        else {
          v match {
            case SeqNestedArray(arr, segs) => {
              val newArr = ea.replicateSeg(count, arr)
              val (ps,ls) = unzip(segs)
              val newLens = intElem.replicateSeg(count, ls)
              val newps = newLens.scan
              SeqNestedArray(newArr, SeqPairArray(newps, newLens))
            }
            case _ => error("expected SeqNestedArray but found " + v)
          }

        }
      }

      def tabulate(len: Int)(f:Int => PA[A]) = {
        val chunks = Array.tabulate(len)(f)
        val lens = intElem.tabulate(len)(i => chunks(i).length)
        val positions = lens.scan
        val segments = positions.zip(lens)
        val arrToSeg = intElem.tabulateSeg(lens.length)(iSeg => intElem.replicate(lens(iSeg), iSeg))
        var arr = ea.tabulate(arrToSeg.length)(i => {
          val iSeg = arrToSeg(i)
          val (p,l) = segments(iSeg)
          val chunk = chunks(iSeg)
          chunk(i - p)
        })
        SeqNestedArray(arr, segments)
      }
      def tabulateSeg(len: Int)(f:Int => PA[PA[A]]) = {
        val temp = Array.concat(Array.tabulate(len)(i => f(i).toArray): _*)
        tabulate(temp.length)(i => temp(i))
      }

      def empty = new SeqNestedArray(ea.empty, segElem.singleton((0,0)))
    }

  def unconcat[A,B](a: PA[PA[A]])(b:PA[B])
                        (implicit ea: Elem[B], epa: Elem[PA[B]]): PA[PA[B]] = a match {
    case nested: NestedArray[A] => SeqNestedArray(b, nested.segments)
    case _ => error("expected NestedArray but was " + a)
  }

  trait SeqPArray[T] extends PArray[T] {
    override def zip[B](b: PArray[B])(implicit eb: Elem[B], etb: Elem[(T, B)]): PArray[(T, B)] = new SeqPairArray(this, b)

    def backPermute(idxs: PArray[Int]): PArray[T] = {   //(implicit t: Elem[T], m:ClassManifest[T])
      val len = idxs.length
      elem.tabulate(len)(i => this(idxs(i)))
    }

    def permute(idxs: PArray[Int]): PArray[T] = {       //(implicit t: Elem[T], m:ClassManifest[T])
      implicit val mt = elem.manifest
      val newArr = new Array(length)
      for (i <- 0 to (idxs.length-1)) newArr(idxs(i)) = this(i)
      elem.fromArray(newArr)
    }

    def flagCombine(ifFalse: PA[T], fs: PA[Boolean]): PA[T] =
        this.zip(ifFalse).zip(fs).map { case ((t,f),flag) => if (flag) t else f }

    def toArray = { implicit val m = elem.manifest; Array.tabulate(length)(i => index(i)) }

    def flatMap[B:Elem](f:T=>PA[B]): PA[B] = element[B].tabulateSeg(this.length)(i => f(this(i)))

    def withFilter(f: T=>Boolean): PA[T] = filter(f)

    def scan(implicit s:Semigroup[T]): PA[T] = {error("scan is not supported for array with elements of type " + this.getClass.getName) }

    def ++(that: PA[T])(implicit epa:Elem[PA[T]]): PA[T] = { concat(element[PA[T]].fromArray(Array(this,that))) }
  }


  case class SeqStdArray[T](override val arr: Array[T])(implicit  t: Elem[T], z:Zero[T])
    extends StdArray[T](arr) with SeqPArray[T] {
    override val elem = t
    implicit val manifest = t.manifest
    override val elem2 = element[(T,T)]

    def length = arr.length
    def index(i: Int) = arr(i)
    override def toArray = arr
    def slice(start:Int, len:Int): PA[T] = t.fromArray(arr.slice(start, start + len))
    override def scan(implicit s:Semigroup[T]): PA[T] = {
      var res = new Array(arr.length)
      if (res.length > 0) {
        res(0) = mzero[T]
        for (i <- 1 until arr.length) res(i) = s.append(res(i-1),arr(i-1))
      }
      t.fromArray(res)
    }

    def map[B:Elem](f: T => B) = {
      element[B].tabulate(arr.length)(i => f(arr(i)))
    }

    // length(this) + length(ifFalse) == length(flags)
    def flagMerge(ifFalse: PA[T], flags: PA[Boolean]) = {
      var ti = 0; var fi = 0
      val len = flags.length
      val res = new Array(len)
      for (i <- 0 until len) {
        if (flags(i)) { res(i) = this(ti); ti += 1 }
        else   { res(i) = ifFalse(fi); fi += 1 }
      }
      SeqStdArray(res)
    }

    def flagSplit(flags: PA[Boolean]) = { // length(this) == length(flags) == (length(A) + length(B))
      var ti = 0; var fi = 0
      val len = flags.length
      val truecount = flags.map((b:Boolean) => if (b) 1 else 0).sum
      val tr = new Array(truecount)
      val fr = new Array(len - truecount)
      for (i <- 0 until len)
        if (flags(i)) { tr(ti) = this(i); ti += 1 }
        else          { fr(fi) = this(i); fi += 1 }
      (SeqStdArray(tr), SeqStdArray(fr))
    }
    override def toString = arr.mkString("(",", ",")")
  }

  case class SeqPairArray[A, B](override val a: PArray[A], override val b: PArray[B])
         (implicit  e:Elem[(A,B)])
          extends PairArray[A,B](a,b) with SeqPArray[(A,B)] {
    //override val manifest = m
    override val elem = e
    override val elem2 = element[((A,B),(A,B))]

    def map[R:Elem](f: ((A,B)) => R): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => f(a(i),b(i)))
    }

    def slice(start: Int, len: Int) = new SeqPairArray(a.slice(start, len), b.slice(start, len))

    override def flagCombine(ifFalse: PA[(A,B)], flags: PA[Boolean]) = ifFalse.asInstanceOf[SeqPairArray[A,B]] match {
      case SeqPairArray(falseA, falseB) => SeqPairArray(a.flagCombine(falseA, flags), b.flagCombine(falseB, flags))
      case _ => error("SeqPairArray expected by was" + ifFalse)
    }

    // length(this) + length(ifFalse) == length(flags)
    def flagMerge(ifFalse: PA[(A,B)], flags: PA[Boolean]) = ifFalse.matchType {
      (arr: SeqPairArray[A,B]) => SeqPairArray(a.flagMerge(arr.a, flags), b.flagMerge(arr.b, flags))
    }

    def flagSplit  (flags: PA[Boolean]) = { // length(this) == length(flags) == (length(A) + length(B))
      val (at,af) = a.flagSplit(flags)
      val (bt,bf) = b.flagSplit(flags)
      (SeqPairArray(at,bt), SeqPairArray(af,bf))
    }
  }

  case class SeqSumArray[A, B](
       override val flags: PArray[Boolean],
       override val a: PArray[A],
       override val b: PArray[B])
      (implicit ea: Elem[A], eb: Elem[B], e: Elem[(A|B)])
       extends SumArray[A, B](flags, a, b) with SeqPArray[(A|B)]
  {
    //override val manifest = m
    override val elem = e
    override val elem2 = element[((A|B),(A|B))]

    lazy val indices: PA[Int] = {
      val aindices = flags.map((b: Boolean) => if (b) 0 else 1).scan
      val bindices = flags.map((b:Boolean) => if (b) 1 else 0).scan
      bindices.flagCombine(aindices, flags)
    }

    def index(i: Int) = {Either.cond(flags(i), b(indices(i)), a(indices(i)))}
    override def toArray = Array.tabulate(flags.length)(i => if (flags(i)) Right(b(i)) else Left(a(i)))

    def map[R:Elem](f: (A|B) => R): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => f(index(i)))
    }

    def slice(start: Int, len: Int) = {
      val fslice = flags.slice(start, len)
      val islice = indices.slice(start, len)
      val (ib, ia) = islice.flagSplit(fslice)
      SeqSumArray(fslice, sliceByIndices(a, ia), sliceByIndices(b, ib))
    }

    // length(this) + length(ifFalse) == length(flags)
    def flagMerge(ifFalse: PA[(A|B)], fs: PA[Boolean]) = ifFalse matchType {
      (arr: SeqSumArray[A,B]) => {
        val newFlags = this.flags.flagMerge(arr.flags, fs)
        val newIndices = this.indices.flagMerge(arr.indices, fs)
        fs.zip(newFlags).zip(newIndices) map {
          case ((isTrueArr,isB),i) =>
            if (isTrueArr) {
              if (isB) Right(b(i)) else Left(a(i))
            }
            else {
              if (isB) Right(arr.b(i)) else Left(arr.a(i))
            }
        }
      }
    }

    // length(this) == length(flags) == (length(A) + length(B))
    def flagSplit  (fs: PA[Boolean]) = {
        val (trueFlags, falseFlags) = this.flags.flagSplit(fs)
        val (trueIndices, falseIndices) = this.indices.flagSplit(fs)
        val trueArr: PA[(A|B)] = trueFlags.zip(trueIndices).map { case (isB,i) => if (isB) Right(b(i)) else Left(a(i)) }
        val falseArr: PA[(A|B)] = falseFlags.zip(falseIndices).map { case (isB,i) => if (isB) Right(b(i)) else Left(a(i)) }
        (trueArr, falseArr)
    }

    private def sliceByIndices[T:Elem](arr:PA[T], indices: PA[Int]): PA[T] = {
      val len = indices.length
      if (len == 0) return element[T].empty
      val first = indices(0)
      arr.slice(first, len)
    }
  }

  case class SeqNestedArray[A](override val arr: PA[A], override val segments: PA[(Int,Int)])
         (implicit ea: Elem[A], epa: Elem[PA[A]])
          extends NestedArray[A](arr,segments)
          with SeqPArray[PA[A]] {
    val intElem = element[Int]

    //override val manifest = m
    override val elem = epa
    override val elem2 = element[(PA[A],PA[A])]

    def map[R:Elem](f: PA[A] => R): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => {val (p,l) = segments(i); f(arr.slice(p,l))})
    }

    def slice(start: Int, len: Int): PA[PA[A]] = {
      if (len == 0) return elem.empty

      val (positions, lens) = unzip(segments.slice(start, len))
      val arrPos = positions(0)
      val arrLen = positions(len-1) + lens(len-1) - arrPos
      val newArr = arr.slice(arrPos, arrLen)
      val newPositions: PA[Int] = positions.zip(intElem.replicate(len, arrPos)).map {case (p:Int,ofs:Int) => p - ofs}
      SeqNestedArray(newArr, newPositions.zip(lens))
    }

    def flagMerge(ifFalse: PA[PA[A]], fs: PA[Boolean]) = ifFalse matchType {
      (arr: SeqNestedArray[A]) => {
        val (thisPositions, thisLens) = unzip(this.segments)
        val (thatPositions, thatLens) = unzip(arr.segments)
        val newLens = thisLens.flagMerge(thatLens, fs)
        val newPositions = thisPositions.flagMerge(thatPositions, fs)
        fs.zip(newPositions.zip(newLens)) map { case (flag,(p,l)) => if (flag) this.arr.slice(p,l) else arr.arr.slice(p,l) }
      }
    }

    def flagSplit  (fs: PA[Boolean]) = {
      val (positions, lens) = unzip(this.segments)
      val (posTrue,posFalse) = positions.flagSplit(fs)
      val (lensTrue,lensFalse) = lens.flagSplit(fs)
      val arrTrue = posTrue.zip(lensTrue) map {case (p,l) => this.arr.slice(p,l)}
      val arrFalse = posFalse.zip(lensFalse) map {case (p,l) => this.arr.slice(p,l)}
      (arrTrue, arrFalse)
    }
  }

  case class SeqUnitArray(override val len: Int) extends UnitArray(len) with SeqPArray[Unit] {
    override val elem = element[Unit]
    //override val manifest = elem.manifest
    override val elem2 = element[(Unit,Unit)]

    def map[R:Elem](f: Unit => R): PArray[R] = {
      element[R].tabulate(len)(i => f())
    }

    def slice(start: Int, l: Int) = new SeqUnitArray(l)
    override def flagCombine(ifFalse: PA[Unit], flags: PA[Boolean]) = this

    // length(this) + length(ifFalse) == length(flags)
    def flagMerge  (ifFalse: PA[Unit], flags: PA[Boolean]): PA[Unit] = SeqUnitArray(flags.length)

    def flagSplit  (flags: PA[Boolean]) = { // length(this) == length(flags) == (length(A) + length(B))
      val truecount = flags.map((b:Boolean) => if (b) 1 else 0).sum
      (SeqUnitArray(truecount), SeqUnitArray(flags.length - truecount))
    }
  }
}
