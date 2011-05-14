package scalan.sequential

import collection.generic.CanBuildFrom
import scala.reflect.ClassManifest
import scalan.common._
import Common._
import scalan.util.Utils
import Utils._
import scalan.dsl._

trait SeqTreeImplementation extends PTrees with SeqImplementation with Arrays {

  implicit def treeElement[A](implicit ea: Elem[A], z: Zero[A]): Elem[Tree[A]] = new Element[Tree[A], PA] {
    implicit val elemTA:Elem[Tree[A]] = this
    implicit val ma: ClassManifest[A] = ea.manifest
    implicit val m: ClassManifest[Tree[A]] = ClassManifest.classType(classOf[Tree[A]], ea.manifest)

    def itemElem: Elem[Item[A]] = element[Item[A]]
    def manifest: ClassManifest[Tree[A]] = m

    def replicate(count: Int, v: Tree[A]): PA[Tree[A]] = {
      if (count == 0) return empty
      val item = (v.value, v.children)
      val items = itemElem.replicate(count, item)
      toTreeArray(items)
    }

    def replicateSeg(count: Int, v: PA[Tree[A]]): PA[Tree[A]] = {
      if (count == 0 || v.length == 0) return empty
      val items = toItems(v).get
      toTreeArray(itemElem.replicateSeg(count, items))
    }


    def tabulate(len: Int)(f:Int => Tree[A]) = {
      if (len == 0) empty
      else {
        val items = itemElem.tabulate(len)(i => {val t = f(i); (t.value, t.children)})
        if (items.length == 0) empty
        else toTreeArray(items)
      }
    }

    def tabulateSeg(len: Int)(f:Int => PA[Tree[A]]) = {
      if (len == 0) empty
      else {
        val items = itemElem.tabulateSeg(len)(i => toItems(f(i)) some {items => items} none itemElem.empty)
        if (items.length == 0) empty
        else toTreeArray(items)
      }
    }
    def empty = SeqTreeArray(None)
  }

  def toItems[A](ts: PA[Tree[A]]):Option[PA[Item[A]]] = ts matchType { (arr: SeqTreeArray[A]) => arr.items }
  def toItems[A](ts: PA[Tree[A]], defItems: => PA[Item[A]]): PA[Item[A]] = toItems(ts) some {items => items} none defItems
  def toTreeArray[A](items: PA[Item[A]])
                    (implicit ea: Elem[A], za: Zero[A]):PA[Tree[A]] = SeqTreeArray(Some(items))

  case class SeqTreeArray[A](items: Option[PA[Item[A]]])
         (implicit ea: Elem[A], za: Zero[A], eta: Elem[Tree[A]])
          extends TreeArray[A](items)
          with SeqPArray[Tree[A]]
  {
    override val elem = eta
    override val elem2 = items ? element[(Tree[A],Tree[A])] | null
    implicit val ma: ClassManifest[A] = ea.manifest
    implicit val bfa: CanBuildFrom[Array[Array[A]], A, Array[A]] = Array.canBuildFrom[A]

    def map[R:Elem](f: Tree[A] => R): PA[R] = {
      val len = length
      element[R].tabulate(len)(i => {val t = this(i); f(t)})
    }

    def slice(start: Int, len: Int): PA[Tree[A]] = {
      if (len == 0) return elem.empty
      items some { (is :PA[Item[A]]) => toTreeArray(is.slice(start, len))} none {error("Cannot slice empty TreeArray " + start + " " + len)}
    }

    def flagMerge(ifFalse: PA[Tree[A]], fs: PA[Boolean]) = {
      val emptyItems = element[Item[A]].empty
      val thisItems = toItems(this, emptyItems)
      val thatItems = toItems(ifFalse, emptyItems)
      toTreeArray(thisItems flagMerge(thatItems, fs))
    }

    def flagSplit(fs: PA[Boolean]) = {
      val emptyItems = element[Item[A]].empty
      val thisItems = toItems(this, emptyItems)
      val (tItems, fItems) = thisItems.flagSplit(fs)
      (toTreeArray(tItems), toTreeArray(fItems))
    }
  }
}
