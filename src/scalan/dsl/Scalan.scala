package scalan.dsl

trait Scalan extends Arrays with PTrees {
    implicit def paManifest[A](implicit ea: Elem[A]): ClassManifest[PA[A]]

}
