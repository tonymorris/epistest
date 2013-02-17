package org.epistest

import scalaz._, Scalaz._, Leibniz._

sealed trait Gen[+A] {
  val value: Size => Rng[A]

  def apply(a: Size): Rng[A] =
    value(a)

  def map[X](f: A => X): Gen[X] =
    Gen(value(_) map f)

  def flatMap[X](f: A => Gen[X]): Gen[X] =
    Gen(s =>
      value(s) flatMap (a => f(a) value s))

  def ap[X](f: Gen[A => X]): Gen[X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: Gen[X]): Gen[(A, X)] =
    zipWith(q)(a => (a, _))

  def zipWith[B, C](r: Gen[B])(f: A => B => C): Gen[C] =
    r.ap(map(f))

  def resume(a: Size): RngResume[A] =
    apply(a).resume

  def run(a: Size): A =
    apply(a).run

  def mapr(f: RngOp ~> RngOp): Gen[A] =
    Gen(value(_) mapr f)

  def mapRng[X](f: Rng[A] => Rng[X]): Gen[X] =
    Gen(f compose value)

  def |+|[AA >: A](x: Gen[AA])(implicit S: Semigroup[AA]): Gen[AA] =
    for {
      a <- this
      b <- x
    } yield S.append(a, b)

  def list: Gen[List[A]] =
    Gen(s => apply(s) list s)

  def list1: Gen[NonEmptyList[A]] =
    Gen(s => apply(s) list1 s)

  def option: Gen[Option[A]] =
    Gen(apply(_).option)

  def ***[X](x: Gen[X]): Gen[(A, X)] =
    zip(x)

  def either[X](x: Gen[X]): Gen[A \/ X] =
    Gen(s => apply(s) either x(s))

  def +++[X](x: Gen[X]): Gen[A \/ X] =
    either(x)

  def eitherS[X](x: Gen[X]): Gen[Either[A, X]] =
    Gen(s => apply(s) eitherS x(s))

  def flatten[AA >: A, B](implicit f: AA === Gen[B]): Gen[B] =
    flatMap(f)

  def function[AA >: A, X](p: Perturb[AA, X]): Gen[X => AA] =
    mapRng(_ function p)

  def endofunction[AA >: A](implicit S: Semigroup[AA]): Gen[AA => AA] =
    mapRng(_.endofunction)

  def endofunctionE[AA >: A](implicit S: Semigroup[AA]): Gen[Endo[AA]] =
    mapRng(_.endofunctionE)


}

object Gen {
  def apply[A](v: Size => Rng[A]): Gen[A] =
    new Gen[A] {
      val value = v
    }

  def get: Gen[Size] =
    Gen(Rng.insert)

  def double: Gen[Double] =
    Rng.double.gen

  def float: Gen[Float] =
    Rng.float.gen

  def long: Gen[Long] =
    Rng.long.gen

  def int: Gen[Int] =
    Rng.int.gen

  def boolean: Gen[Boolean] =
    Rng.boolean.gen

  def positiveint: Gen[Int] =
    Rng.positiveint.gen

  def negativeint: Gen[Int] =
    Rng.negativeint.gen

  def digit: Gen[Digit] =
    Rng.digit.gen

  def digits: Gen[List[Digit]] =
    Rng.digit.list

  def digits1: Gen[NonEmptyList[Digit]] =
    Rng.digit.list1

  def numeric: Gen[Char] =
    Rng.numeric.gen

  def numerics: Gen[List[Char]] =
    Rng.numeric.list

  def numerics1: Gen[NonEmptyList[Char]] =
    Rng.numeric.list1

  def char: Gen[Char] =
    Rng.char.gen

  def chars: Gen[List[Char]] =
    Rng.char.list

  def chars1: Gen[NonEmptyList[Char]] =
    Rng.char.list1

  def upper: Gen[Char] =
    Rng.upper.gen

  def uppers: Gen[List[Char]] =
    Rng.upper.list

  def uppers1: Gen[NonEmptyList[Char]] =
    Rng.upper.list1

  def lower: Gen[Char] =
    Rng.lower.gen

  def lowers: Gen[List[Char]] =
    Rng.lower.list

  def lowers1: Gen[NonEmptyList[Char]] =
    Rng.lower.list1

  def alpha: Gen[Char] =
    Rng.alpha.gen

  def alphas: Gen[List[Char]] =
    Rng.alpha.list

  def alphas1: Gen[NonEmptyList[Char]] =
    Rng.alpha.list1

  def alphanumeric: Gen[Char] =
    Rng.alphanumeric.gen

  def alphanumerics: Gen[List[Char]] =
    Rng.alphanumeric.list

  def alphanumerics1: Gen[NonEmptyList[Char]] =
    Rng.alphanumeric.list1

  def string: Gen[String] =
    Gen(Rng.string)

  def string1: Gen[String] =
    Gen(Rng.string1)

  def upperstring: Gen[String] =
    Gen(Rng.upperstring)

  def upperstring1: Gen[String] =
    Gen(Rng.upperstring1)

  def lowerstring: Gen[String] =
    Gen(Rng.lowerstring)

  def lowerstring1: Gen[String] =
    Gen(Rng.lowerstring1)

  def alphastring: Gen[String] =
    Gen(Rng.alphastring)

  def alphastring1: Gen[String] =
    Gen(Rng.alphastring1)

  def numericstring: Gen[String] =
    Gen(Rng.numericstring)

  def numericstring1: Gen[String] =
    Gen(Rng.numericstring1)

  def alphanumericstring: Gen[String] =
    Gen(Rng.alphanumericstring)

  def alphanumericstring1: Gen[String] =
    Gen(Rng.alphanumericstring1)

  def identifier: Gen[NonEmptyList[Char]] =
    Gen(Rng.identifier)

  def identifierstring: Gen[String] =
    Gen(Rng.identifierstring)

  def pair[A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] =
    a zip b

  def triple[A, B, C](a: Gen[A], b: Gen[B], c: Gen[C]): Gen[(A, B, C)] =
    for {
      aa <- a
      bb <- b
      cc <- c
    } yield (aa, bb, cc)

  def insert[A](a: A): Gen[A] =
    Rng.insert(a).gen

  def chooseLong(l: Long, h: Long): Gen[Long] =
    Rng.chooseLong(l, h).gen

  def chooseDouble(l: Double, h: Double): Gen[Double] =
    Rng.chooseDouble(l, h).gen

  def chooseFloat(l: Float, h: Float): Gen[Float] =
    Rng.chooseFloat(l, h).gen

  def chooseInt(l: Int, h: Int): Gen[Int] =
    Rng.chooseInt(l, h).gen

  def oneofL[A](x: NonEmptyList[A]): Gen[A] =
    Rng.oneofL(x).gen

  def oneof[A](a: A, as: A*): Gen[A] =
    Rng.oneof(a, as: _*).gen

  def sequence[T[_], A](x: T[Gen[A]])(implicit T: Traverse[T]): Gen[T[A]] =
    T.sequence(x)

  def sequencePair[X, A](x: X, r: Gen[A]): Gen[(X, A)] =
    sequence[({type f[x] = (X, x)})#f, A]((x, r))

  def frequencyL[A](x: NonEmptyList[(Int, Rng[A])]): Gen[A] =
    Rng.frequencyL(x).gen

  implicit val GenMonad: Monad[Gen] =
    new Monad[Gen] {
      def bind[A, B](a: Gen[A])(f: A => Gen[B]) =
        a flatMap f
      def point[A](a: => A) =
        insert(a)
    }

  implicit def GenSemigroup[A](implicit S: Semigroup[A]): Semigroup[Gen[A]] =
    new Semigroup[Gen[A]] {
      def append(r1: Gen[A], r2: => Gen[A]) =
        r1 |+| r2
    }

  implicit def GenMonoid[A](implicit M: Monoid[A]): Monoid[Gen[A]] =
    new Monoid[Gen[A]] {
      def append(r1: Gen[A], r2: => Gen[A]) =
        r1 |+| r2

      def zero =
        insert(M.zero)
    }

}
