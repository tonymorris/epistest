package org.epistest

import scalaz._

sealed trait Gen[-A, +B] {
  val value: A => Rng[B]

  def apply(a: A): Rng[B] =
    value(a)

  def map[X](f: B => X): Gen[A, X] =
    Gen(value(_) map f)

  def flatMap[AA <: A, X](f: B => Gen[AA, X]): Gen[AA, X] =
    Gen(a => value(a) flatMap (b => f(b) value a))

  def ap[AA <: A, X](f: Gen[AA, B => X]): Gen[AA, X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[AA <: A, X](q: Gen[AA, X]): Gen[AA, (B, X)] =
    for {
      b <- this
      x <- q
    } yield (b, x)

  def resume(a: A): RngResume[B] =
    value(a).resume

  def run(a: A): B =
    value(a).run

  def mapr(f: RngOp ~> RngOp): Gen[A, B] =
    Gen(value(_) mapr f)

  def mapRng[X](f: Rng[B] => Rng[X]): Gen[A, X] =
    Gen(f compose value)

  def flatMapRng[AA <: A, X](f: Rng[B] => Gen[AA, X]): Gen[AA, X] =
    Gen(a => f(value(a)) value a)

}

object Gen {
  private[epistest] def apply[A, B](v: A => Rng[B]): Gen[A, B] =
    new Gen[A, B] {
      val value = v
    }

  def insert[A, B](b: => B): Gen[A, B] =
    apply(_ => Rng.insert(b))
}