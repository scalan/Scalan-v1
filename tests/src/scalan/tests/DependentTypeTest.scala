package scalan.tests

import org.junit.{Test}
import org.junit.Assert._
import scalan.dsl._
import scalan.sequential._
import org.hamcrest._
import core._
import Is._
import scalan.util.{ArrayFormatting => PA}
import scalan.samples._

class DependentTypeTest {

  @Test def parallelArraysAreDependentlyTyped = {
    val arr = Array(1,2,3)
    object module1 extends SeqArrays with SeqTreeImplementation
    object module2 extends SeqArrays with SeqTreeImplementation

    //import module1._                              (1)
    //import module2._                              (2)
    //implicit val intElem = module1.element[Int]   (3)
    //import module1.intElement

//    var arr1 = module1.fromArray(arr)
//    var arr2 = module2.fromArray(arr)
//    var arr3 = arr1 ++ arr2

  }

}
