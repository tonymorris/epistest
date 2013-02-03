package org.epistest

// belongs in scalaz
sealed trait ToOption[-A, +B] {
  val value: A => Option[B]

  def map[X](f: B => X): ToOption[A, X] =
    ToOption(value(_) map f)

  def flatMap[AA <: A, X](f: B => ToOption[AA, X]): ToOption[AA, X] =
    ToOption(a => value(a) flatMap (f(_) value a))
}

object ToOption {
  def apply[A, B](v: A => Option[B]): ToOption[A, B] =
    new ToOption[A, B] {
      val value = v
    }

  def function[A, B](f: A => B): ToOption[A, B] =
    apply(a => Some(f(a)))

  def insert[A, B](b: => B): ToOption[A, B] =
    function(_ => b)

  def option[A, B](o: => Option[B]): ToOption[A, B] =
    apply(_ => o)

  def none[A, B]: ToOption[A, B] =
    apply(_ => None)
}
