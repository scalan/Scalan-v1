package scalan.sequential

import reflect.ClassManifest

trait ScalanSequential extends SeqArrays with SeqTreeImplementation {

  implicit def paManifest[A](implicit ea: Elem[A]): ClassManifest[PA[A]] =
      ClassManifest.classType(classOf[PA[A]], ea.manifest)

}
