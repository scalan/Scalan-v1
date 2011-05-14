package scalan.dsl

import scala.text._
import Document._
import scalan.util.ArrayFormatting._
import scalan.common._
import Common._

trait PTrees extends ArraysBase {
  case class Tree[A](value: A, children: PA[Tree[A]]) {
    def toDoc = group(("Tree(" + value + ",") :/: nest(2, children.toDoc) :: ")" :: ED)
  }
  type Item[A] = (A, PA[Tree[A]])

  object Tree {
    def apply[A](v: A)(implicit ea: Elem[A], z: Zero[A]) = new Tree(v, emptyArrayOf[Tree[A]])
  }

  abstract class TreeArray[A](items: Option[PA[Item[A]]]) extends PArray[Tree[A]] {
    self: PA[Tree[A]] =>
    def length = items some {_.length} none 0
    def index(i: Int) = items some {(is: PA[Item[A]]) => {val (v,c) = is(i); Tree(v,c) }} none { error("Cannot get index from empty TreeArray " + i)}
    override def toDoc = items match { case Some(items) => "TreeArray(Some(" :: items.toDoc :: "))" :: ED case None => text("TreeArray(None)") }
  }

  implicit def treeElement[A](implicit a: Elem[A], z: Zero[A]): Elem[Tree[A]]

  def toTreeZero[A](eta:Elem[Tree[A]])(
                implicit za:Zero[A], ea: Elem[A]): Zero[Tree[A]] = zero(Tree(mzero[A], eta.empty))

  implicit def treeZero[A](implicit za:Zero[A], ea: Elem[A], eta:Elem[Tree[A]]): Zero[Tree[A]] = toTreeZero(eta)

  def toItems[A](ts: PA[Tree[A]]):Option[PA[Item[A]]]

  def unzipTree[A](ts: PA[Tree[A]])(implicit ea: Elem[A], ept: Elem[PA[Tree[A]]]): (PA[A], PA[PA[Tree[A]]]) = {
    toItems(ts) match  {
      case Some(items) => unzip(items)
      case None => (ea.empty, ept.empty)
    }
  }


}
