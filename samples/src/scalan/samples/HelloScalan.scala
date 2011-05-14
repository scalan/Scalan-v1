package scalan.samples

import scalan.dsl._

trait HelloScalan extends Scalan {
  def Hello(names: Array[String]) = {
    fromArray(names) map {name => "Hello, " + name + "!"} toArray
  }
}

import scalan.sequential._

object Sample extends HelloScalan with ScalanSequential {

}

// scala> import scalan.samples._
// scala> MySample.HelloScalan(Array("Alex", "Ilya"))
