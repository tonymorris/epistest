package org.epistest

import scalaz._

import Gen._

sealed trait Gen[+A] {
  val value: Size => Rng[A]

  def apply(a: Size): Rng[A] =
    value(a)

  def map[X](f: A => X): Gen[X] =
    Gen(value(_) map f)

  def flatMap[X](f: A => Gen[X]): Gen[X] =
    Gen(a => value(a) flatMap (b => f(b) value a))

  def ap[X](f: Gen[A => X]): Gen[X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: Gen[X]): Gen[(A, X)] =
    for {
      b <- this
      x <- q
    } yield (b, x)

  def resume(a: Size): RngResume[A] =
    value(a).resume

  def run(a: Size): A =
    value(a).run

  def mapr(f: RngOp ~> RngOp): Gen[A] =
    Gen(value(_) mapr f)

  def mapRng[X](f: Rng[A] => Rng[X]): Gen[X] =
    Gen(f compose value)

  def flatMapRng[X](f: Rng[A] => Gen[X]): Gen[X] =
    Gen(a => f(value(a)) value a)

}

object Gen {
  type Size = Int
  private[epistest] def apply[A](v: Size => Rng[A]): Gen[A] =
    new Gen[A] {
      val value = v
    }

  def insert[A](a: => A): Gen[A] =
    apply(_ => Rng.insert(a))
}