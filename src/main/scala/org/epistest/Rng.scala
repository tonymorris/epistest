package org.epistest

import scalaz._, Free._, Scalaz._, NonEmptyList._, Digit._, Leibniz._

sealed trait Rng[+A] {
  val free: Free[RngOp, A]

  import Rng._

  def map[B](f: A => B): Rng[B] =
    Rng(free map f)

  def flatMap[B](f: A => Rng[B]): Rng[B] =
    Rng(free flatMap (f(_).free))

  def ap[X](f: Rng[A => X]): Rng[X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: Rng[X]): Rng[(A, X)] =
    zipWith(q)(a => (a, _))

  def zipWith[B, C](r: Rng[B])(f: A => B => C): Rng[C] =
    r.ap(map(f))

  def foldRun[B, AA >: A](b: B)(f: (B, RngOp[Rng[AA]]) => (B, Rng[AA])): (B, AA) =
    free.foldRun[B, AA](b)((bb, t) => f(bb, t map (Rng(_))) :-> (_.free))

  def resume: RngResume[A] =
    free.resume match {
      case -\/(x) => RngCont(x map (Rng(_)))
      case \/-(x) => RngTerm(x)
    }

  def run(s: Seed = Seed.defaultseed): A = {
    class SeededNextBitsRandom(d: Long) extends java.util.Random(d) {
      def nextbits(bits: Int): Int =
        super.next(bits)
    }

    class NextBitsRandom extends java.util.Random {
      def nextbits(bits: Int): Int =
        super.next(bits)
    }

    @annotation.tailrec
    def loop(g: Rng[A], r: NextBitsRandom): A =
      g.resume match {
        case RngCont(op) =>
          loop(op.next(r nextbits op.bits), r)
        case RngTerm(a) =>
          a
      }

    @annotation.tailrec
    def seededloop(g: Rng[A], r: SeededNextBitsRandom): A =
      g.resume match {
        case RngCont(op) =>
          seededloop(op.next(r nextbits op.bits), r)
        case RngTerm(a) =>
          a
      }

    s.option match {
      case None => loop(this, new NextBitsRandom)
      case Some(t) => seededloop(this, new SeededNextBitsRandom(t))
    }

  }

  def maph[G[+_]](f: RngOp ~> G)(implicit G: Functor[G]): Free[G, A] =
    free mapSuspension f

  def mapr(f: RngOp ~> RngOp): Rng[A] =
    Rng(free mapFirstSuspension f)

  def go[AA >: A](f: RngOp[Rng[AA]] => Rng[AA]): AA =
    free.go[AA](r => f(r map (Rng(_))).free)

  def gen: Gen[A] =
    Gen.readsize(_ => this)

  def |+|[AA >: A](x: Rng[AA])(implicit S: Semigroup[AA]): Rng[AA] =
    for {
      a <- this
      b <- x
    } yield S.append(a, b)

  def many: Gen[List[A]] =
    Gen.readsize(s =>
      for {
        n <- s.value match {
               case None => int
               case Some(y) => chooseInt(0, y)
             }
        a <- sequence(List.fill(n)(this))
      } yield a
    )

  def many1: Gen[NonEmptyList[A]] =
    Gen.readsize(s =>
      for {
        n <- s.value match {
               case None => int
               case Some(y) => chooseInt(0, y)
             }
        z <- this
        a <- sequence(List.fill(n)(this))
      } yield nel(z, a)
    )

  def option: Rng[Option[A]] =
    boolean flatMap (p => sequence[Option, A](if(p) None else Some(this)))

  def ***[X](x: Rng[X]): Rng[(A, X)] =
    zip(x)

  def either[X](x: Rng[X]): Rng[A \/ X] =
    boolean flatMap (p => if(p) map(_.left) else x map (_.right))

  def +++[X](x: Rng[X]): Rng[A \/ X] =
    either(x)

  def eitherS[X](x: Rng[X]): Rng[Either[A, X]] =
    either(x) map (_.toEither)

  def flatten[AA >: A, B](implicit f: AA === Rng[B]): Rng[B] =
    flatMap(f)

}

object Rng {
  private[epistest] def apply[A](f: Free[RngOp, A]): Rng[A] =
    new Rng[A] {
      val free = f
    }

  def bits(n: Int): Rng[Int] =
    RngOp(n, x => x).lift

  def double: Rng[Double] =
    for {
      a <- bits(27)
      b <- bits(26)
    } yield (b.toLong << a) / (1.toLong << 53).toDouble

  def float: Rng[Float] =
    bits(24) map (_ / (1 << 24).toFloat)

  def long: Rng[Long] =
    for {
      a <- bits(32)
      b <- bits(32)
    } yield (a.toLong << 32) + b

  def int: Rng[Int] =
    bits(32)

  def boolean: Rng[Boolean] =
    chooseInt(0, 1) map (_ == 0)

  def positiveint: Rng[Int] =
    int map (math.abs(_))

  def negativeint: Rng[Int] =
    int map (n => if(n > 0) n else -n)

  def digit: Rng[Digit] =
    chooseInt(0, 9) map mod10Digit

  def digits(z: Size, t: Seed): Rng[List[Digit]] =
    digit many (z, t)

  def digits1(z: Size, t: Seed): Rng[NonEmptyList[Digit]] =
    digit many1 (z, t)

  def numeric: Rng[Char] =
    digit map (_.toChar)

  def numerics(z: Size, t: Seed): Rng[List[Char]] =
    numeric many (z, t)

  def numerics1(z: Size, t: Seed): Rng[NonEmptyList[Char]] =
    numeric many1 (z, t)

  def char: Rng[Char] =
    int map (_.toChar)

  def chars(z: Size, t: Seed): Rng[List[Char]] =
    char many (z, t)

  def chars1(z: Size, t: Seed): Rng[NonEmptyList[Char]] =
    char many1 (z, t)

  def upper: Rng[Char] =
    chooseInt(65, 90) map (_.toChar)

  def uppers(z: Size, t: Seed): Rng[List[Char]] =
    upper many (z, t)

  def uppers1(z: Size, t: Seed): Rng[NonEmptyList[Char]] =
    upper many1 (z, t)

  def lower: Rng[Char] =
    chooseInt(97, 122) map (_.toChar)

  def lowers(z: Size, t: Seed): Rng[List[Char]] =
    lower many (z, t)

  def lowers1(z: Size, t: Seed): Rng[NonEmptyList[Char]] =
    lower many1 (z, t)

  def alpha: Rng[Char] =
    upper +++ lower map {
      case -\/(c) => c
      case \/-(c) => c
    }

  def alphas(z: Size, t: Seed): Rng[List[Char]] =
    alpha many (z, t)

  def alphas1(z: Size, t: Seed): Rng[NonEmptyList[Char]] =
    alpha many1 (z, t)

  def alphanumeric: Rng[Char] =
    chooseInt(0, 61) map (c =>
      (if(c <= 25)
        c + 65
      else if(c <= 51)
        c + 71
      else
        c - 4).toChar)

  def alphanumerics(z: Size, t: Seed): Rng[List[Char]] =
    alphanumeric many (z, t)

  def alphanumerics1(z: Size, t: Seed): Rng[NonEmptyList[Char]] =
    alphanumeric many1 (z, t)

  def string(z: Size, t: Seed): Rng[String] =
    chars(z, t) map (_.mkString)

  def string1(z: Size, t: Seed): Rng[String] =
    chars1(z, t) map (_.toList.mkString)

  def upperstring(z: Size, t: Seed): Rng[String] =
    uppers(z, t) map (_.mkString)

  def upperstring1(z: Size, t: Seed): Rng[String] =
    uppers1(z, t) map (_.toList.mkString)

  def lowerstring(z: Size, t: Seed): Rng[String] =
    lowers(z, t) map (_.mkString)

  def lowerstring1(z: Size, t: Seed): Rng[String] =
    lowers1(z, t) map (_.toList.mkString)

  def alphastring(z: Size, t: Seed): Rng[String] =
    alphas(z, t) map (_.mkString)

  def alphastring1(z: Size, t: Seed): Rng[String] =
    alphas1(z, t) map (_.toList.mkString)

  def numericstring(z: Size, t: Seed): Rng[String] =
    numerics(z, t) map (_.mkString)

  def numericstring1(z: Size, t: Seed): Rng[String] =
    numerics1(z, t) map (_.toList.mkString)

  def alphanumericstring(z: Size, t: Seed): Rng[String] =
    alphanumerics(z, t) map (_.mkString)

  def alphanumericstring1(z: Size, t: Seed): Rng[String] =
    alphanumerics1(z, t) map (_.toList.mkString)

  def identifier(z: Size, t: Seed): Rng[NonEmptyList[Char]] =
    for {
      a <- alpha
      b <- alphanumerics(if(z exists (_ < 1)) z.inc else z.dec, t)
    } yield nel(a, b)

  def identifierstring(z: Size, t: Seed): Rng[String] =
    identifier(z, t) map (_.toList.mkString)

  def pair[A, B](a: Rng[A], b: Rng[B]): Rng[(A, B)] =
    a zip b

  def triple[A, B, C](a: Rng[A], b: Rng[B], c: Rng[C]): Rng[(A, B, C)] =
    for {
      aa <- a
      bb <- b
      cc <- c
    } yield (aa, bb, cc)

  def insert[A](a: A): Rng[A] =
    Rng(Return(a))

  def chooseLong(l: Long, h: Long): Rng[Long] =
    long map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      ll + math.abs(x % (hh - ll + 1))
    })

  def chooseDouble(l: Double, h: Double): Rng[Double] =
    double map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      val diff = hh - ll
      if(diff == 0)
        ll
      else
        ll + math.abs(x * diff + ll)
    })

  def chooseFloat(l: Float, h: Float): Rng[Float] =
    float map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      val diff = hh - ll
      if(diff == 0)
        ll
      else
        ll + math.abs(x * diff + ll)
    })

  def chooseInt(l: Int, h: Int): Rng[Int] =
    int map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      ll + math.abs(x % (hh - ll + 1))
    })

  def oneofL[A](x: NonEmptyList[A]): Rng[A] =
    chooseInt(0, x.length - 1) map (x toList _)

  def oneof[A](a: A, as: A*): Rng[A] =
    oneofL(NonEmptyList(a, as: _*))

  def sequence[T[_], A](x: T[Rng[A]])(implicit T: Traverse[T]): Rng[T[A]] =
    T.sequence(x)

  def sequencePair[X, A](x: X, r: Rng[A]): Rng[(X, A)] =
    sequence[({type f[x] = (X, x)})#f, A]((x, r))

  def distribute[A, B](a: Rng[A => B]): A => Rng[B] =
    w => a map (_(w))

  def frequencyL[A](x: NonEmptyList[(Int, Rng[A])]): Rng[A] = {
    val t = x.foldLeft(0) {
      case (a, (b, _)) => a + b
    }

    @annotation.tailrec
    def pick(n: Int, l: NonEmptyList[(Int, Rng[A])]): Rng[A] = {
      val (q, r) = l.head
      if(n <= q)
        r
      else l.tail match {
        case Nil => r
        case e::es => pick(n - q, nel(e, es))
      }
    }

    for {
      n <- chooseInt(1, t)
      w <- pick(n, x)
    } yield w
  }

  implicit val RngMonad: Monad[Rng] =
    new Monad[Rng] {
      def bind[A, B](a: Rng[A])(f: A => Rng[B]) =
        a flatMap f
      def point[A](a: => A) =
        insert(a)
    }

  implicit def RngSemigroup[A](implicit S: Semigroup[A]): Semigroup[Rng[A]] =
    new Semigroup[Rng[A]] {
      def append(r1: Rng[A], r2: => Rng[A]) =
        r1 |+| r2
    }

  implicit def RngMonoid[A](implicit M: Monoid[A]): Monoid[Rng[A]] =
    new Monoid[Rng[A]] {
      def append(r1: Rng[A], r2: => Rng[A]) =
        r1 |+| r2

      def zero =
        insert(M.zero)
    }

}
