package org.epistest

import scalaz._

sealed trait Gen[~>[-_, +_], -A, +B] {
  val value: A ~> Rng[B]

  def map[X](f: B => X)(implicit A: Arrow[~>]): Gen[~>, A, X] =
    Gen(A.mapsnd(value)(_ map f))

}

object Gen {
  type RGen[-A, +B] =
  Gen[Function1, A, B]

  type OptionGen[-A, +B] =
  Gen[ToOption, A, B]

  private[epistest] def apply[~>[-_, +_], A, B](v: A ~> Rng[B]): Gen[~>, A, B] =
    new Gen[~>, A, B] {
      val value = v
    }
}