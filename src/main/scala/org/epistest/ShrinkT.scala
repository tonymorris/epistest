package org.epistest

import scalaz._, Scalaz._

sealed trait ShrinkT[F[+_], A] {
  val shrink: A => F[EphemeralStream[A]]


}

object ShrinkT {
  type Shrink[A] =
    ShrinkT[Id, A]
}
