package org.epistest

sealed trait Gen[-A, +B] {
  val value: A => Rng[B]

  def map[X](f: B => X): Gen[A, X] =
    Gen(value(_) map f)

  def flatMap[AA <: A, X](f: B => Gen[AA, X]): Gen[AA, X] =
    Gen(a => value(a) flatMap (b => f(b) value a))

}

object Gen {
  private[epistest] def apply[A, B](v: A => Rng[B]): Gen[A, B] =
    new Gen[A, B] {
      val value = v
    }
}