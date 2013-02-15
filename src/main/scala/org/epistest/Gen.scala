package org.epistest

import scalaz._, Scalaz._, Leibniz._

sealed trait Gen[+A] {
  val value: (Size, Seed) => Rng[GenResult[A]]

  def apply(a: Size, t: Seed): Rng[A] =
    value(a, t) map (_.value)

  def map[X](f: A => X): Gen[X] =
    Gen(value(_, _) map (_ map f))

  def flatMap[X](f: A => Gen[X]): Gen[X] =
    Gen((s, t) =>
      value(s, t) flatMap (a => f(a.value) value (a.size, a.seed)))

  def ap[X](f: Gen[A => X]): Gen[X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: Gen[X]): Gen[(A, X)] =
    zipWith(q)(a => (a, _))

  def zipWith[B, C](r: Gen[B])(f: A => B => C): Gen[C] =
    r.ap(map(f))

  def resume(a: Size, t: Seed): RngResume[A] =
    apply(a, t).resume

  def run(a: Size, t: Seed): A =
    apply(a, t) run t

  def mapr(f: RngOp ~> RngOp): Gen[A] =
    Gen(value(_, _) mapr f)

  def mapResult[X](f: GenResult[A] => GenResult[X]): Gen[X] =
    Gen(value(_, _) map f)

  def flatMapRng[X](f: Rng[GenResult[A]] => Gen[X]): Gen[X] =
    Gen((a, t) => f(value(a, t)) value (a, t))

  def |+|[AA >: A](x: Gen[AA])(implicit S: Semigroup[AA]): Gen[AA] =
    for {
      a <- this
      b <- x
    } yield S.append(a, b)

  def many: Gen[List[A]] =
    Gen.read((s, t) => apply(s, t) many (s, t))

  def many1: Gen[NonEmptyList[A]] =
    Gen.read((s, t) => apply(s, t) many1 (s, t))

  def option: Gen[Option[A]] =
    Gen.read(apply(_, _).option)

  def ***[X](x: Gen[X]): Gen[(A, X)] =
    zip(x)

  def either[X](x: Gen[X]): Gen[A \/ X] =
    Gen.read((s, t) => apply(s, t) either x(s, t))

  def +++[X](x: Gen[X]): Gen[A \/ X] =
    either(x)

  def eitherS[X](x: Gen[X]): Gen[Either[A, X]] =
    Gen.read((s, t) => apply(s, t) eitherS x(s, t))

  def flatten[AA >: A, B](implicit f: AA === Gen[B]): Gen[B] =
    flatMap(f)

}

object Gen {
  def apply[A](v: (Size, Seed) => Rng[GenResult[A]]): Gen[A] =
    new Gen[A] {
      val value = v
    }

  def read[A](v: (Size, Seed) => Rng[A]): Gen[A] =
    apply((s, t) => v(s, t) map (GenResult(s, t, _)))

  def readsize[A](v: Size => Rng[A]): Gen[A] =
    read((s, _) => v(s))

  def readseed[A](v: Seed => Rng[A]): Gen[A] =
    read((_, t) => v(t))

  def getsize: Gen[Size] =
    readsize(Rng.insert)

  def putsize(s: Size): Gen[Unit] =
    apply((_, t) => Rng.insert(GenResult(s, t, ())))

  def getseed: Gen[Seed] =
    readseed(Rng.insert)

  def putseed(s: Seed): Gen[Unit] =
    apply((d, _) => Rng.insert(GenResult(d, s, ())))

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
    Rng.digit.many

  def digits1: Gen[NonEmptyList[Digit]] =
    Rng.digit.many1

  def numeric: Gen[Char] =
    Rng.numeric.gen

  def numerics: Gen[List[Char]] =
    Rng.numeric.many

  def numerics1: Gen[NonEmptyList[Char]] =
    Rng.numeric.many1

  def char: Gen[Char] =
    Rng.char.gen

  def chars: Gen[List[Char]] =
    Rng.char.many

  def chars1: Gen[NonEmptyList[Char]] =
    Rng.char.many1

  def upper: Gen[Char] =
    Rng.upper.gen

  def uppers: Gen[List[Char]] =
    Rng.upper.many

  def uppers1: Gen[NonEmptyList[Char]] =
    Rng.upper.many1

  def lower: Gen[Char] =
    Rng.lower.gen

  def lowers: Gen[List[Char]] =
    Rng.lower.many

  def lowers1: Gen[NonEmptyList[Char]] =
    Rng.lower.many1

  def alpha: Gen[Char] =
    Rng.alpha.gen

  def alphas: Gen[List[Char]] =
    Rng.alpha.many

  def alphas1: Gen[NonEmptyList[Char]] =
    Rng.alpha.many1

  def alphanumeric: Gen[Char] =
    Rng.alphanumeric.gen

  def alphanumerics: Gen[List[Char]] =
    Rng.alphanumeric.many

  def alphanumerics1: Gen[NonEmptyList[Char]] =
    Rng.alphanumeric.many1

  def string: Gen[String] =
    Gen.read(Rng.string)

  def string1: Gen[String] =
    Gen.read(Rng.string1)

  def upperstring: Gen[String] =
    Gen.read(Rng.upperstring)

  def upperstring1: Gen[String] =
    Gen.read(Rng.upperstring1)

  def lowerstring: Gen[String] =
    Gen.read(Rng.lowerstring)

  def lowerstring1: Gen[String] =
    Gen.read(Rng.lowerstring1)

  def alphastring: Gen[String] =
    Gen.read(Rng.alphastring)

  def alphastring1: Gen[String] =
    Gen.read(Rng.alphastring1)

  def numericstring: Gen[String] =
    Gen.read(Rng.numericstring)

  def numericstring1: Gen[String] =
    Gen.read(Rng.numericstring1)

  def alphanumericstring: Gen[String] =
    Gen.read(Rng.alphanumericstring)

  def alphanumericstring1: Gen[String] =
    Gen.read(Rng.alphanumericstring1)

  def identifier: Gen[NonEmptyList[Char]] =
    Gen.read(Rng.identifier)

  def identifierstring: Gen[String] =
    Gen.read(Rng.identifierstring)

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
