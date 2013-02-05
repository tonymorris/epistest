package org.epistest

import scalaz._, Free._, Scalaz._, NonEmptyList._, Digit._

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
    for {
      a <- this
      x <- q
    } yield (a, x)

  def zap[G[+_], B](fs: Cofree[G, A => B])(implicit G: Functor[G], d: Zap[RngOp, G]): B =
    free.zap(fs)

  def foldRun[B, AA >: A](b: B)(f: (B, RngOp[Rng[AA]]) => (B, Rng[AA])): (B, AA) =
    free.foldRun[B, AA](b)((bb, t) => f(bb, t map (Rng(_))) :-> (_.free))

  def resume: RngResume[A] =
    free.resume match {
      case -\/(x) => RngCont(x map (Rng(_)))
      case \/-(x) => RngTerm(x)
    }

  def run: A = {
    @annotation.tailrec
    def loop(g: Rng[A], r: java.util.Random): A =
      g.resume match {
        case RngCont(NextDouble(q)) =>
          loop(q(r.nextDouble), r)
        case RngCont(NextLong(q)) =>
          loop(q(r.nextLong), r)
        case RngCont(NextInt(q)) =>
          loop(q(r.nextInt), r)
        case RngTerm(a) =>
          a
      }

    loop(this, new java.util.Random)
  }

  def maph[G[+_]](f: RngOp ~> G)(implicit G: Functor[G]): Free[G, A] =
    free mapSuspension f

  def mapr(f: RngOp ~> RngOp): Rng[A] =
    Rng(free mapFirstSuspension f)

  def go[AA >: A](f: RngOp[Rng[AA]] => Rng[AA]): AA =
    free.go[AA](r => f(r map (Rng(_))).free)

  def gen[X]: Gen[X, A] =
    Gen(_ => this)

  def |+|[AA >: A](x: Rng[AA])(implicit S: Semigroup[AA]): Rng[AA] =
    for {
      a <- this
      b <- x
    } yield S.append(a, b)

  def many(z: Int): Rng[List[A]] =
    for {
      i <- chooseInt(0, z)
      q <- sequence(List.fill(i)(this))
    } yield q

  def many1(z: Int): Rng[NonEmptyList[A]] =
    for {
      i <- chooseInt(0, z)
      p <- this
      q <- sequence(List.fill(i)(this))
    } yield nel(p, q)

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
}

object Rng {
  private[epistest] def apply[A](f: Free[RngOp, A]): Rng[A] =
    new Rng[A] {
      val free = f
    }

  def double: Rng[Double] =
    NextDouble(x => x).lift

  def long: Rng[Long] =
    NextLong(x => x).lift

  def int: Rng[Int] =
    NextInt(x => x).lift

  def boolean: Rng[Boolean] =
    chooseInt(0, 1) map (_ == 0)

  def positiveint: Rng[Int] =
    int map (math.abs(_))

  def negativeint: Rng[Int] =
    int map (n => if(n > 0) -n else n)

  def digit: Rng[Digit] =
    chooseInt(0, 9) map mod10Digit

  def digits(z: Int): Rng[List[Digit]] =
    digit many z

  def digits1(z: Int): Rng[NonEmptyList[Digit]] =
    digit many1 z

  def numeric: Rng[Char] =
    digit map (_.toChar)

  def numerics(z: Int): Rng[List[Char]] =
    numeric many z

  def numerics1(z: Int): Rng[NonEmptyList[Char]] =
    numeric many1 z

  def char: Rng[Char] =
    int map (_.toChar)

  def chars(z: Int): Rng[List[Char]] =
    char many z

  def chars1(z: Int): Rng[NonEmptyList[Char]] =
    char many1 z

  def upper: Rng[Char] =
    chooseInt(65, 90) map (_.toChar)

  def uppers(z: Int): Rng[List[Char]] =
    upper many z

  def uppers1(z: Int): Rng[NonEmptyList[Char]] =
    upper many1 z

  def lower: Rng[Char] =
    chooseInt(97, 122) map (_.toChar)

  def lowers(z: Int): Rng[List[Char]] =
    lower many z

  def lowers1(z: Int): Rng[NonEmptyList[Char]] =
    lower many1 z

  def alpha: Rng[Char] =
    upper +++ lower map {
      case -\/(c) => c
      case \/-(c) => c
    }

  def alphas(z: Int): Rng[List[Char]] =
    alpha many z

  def alphas1(z: Int): Rng[NonEmptyList[Char]] =
    alpha many1 z

  def alphanumeric: Rng[Char] =
    chooseInt(0, 61) map (c =>
      (if(c <= 25)
        c + 65
      else if(c <= 51)
        c + 71
      else
        c - 4).toChar)

  def alphanumerics(z: Int): Rng[List[Char]] =
    alphanumeric many z

  def alphanumerics1(z: Int): Rng[NonEmptyList[Char]] =
    alphanumeric many1 z

  def string(z: Int): Rng[String] =
    chars(z) map (_.mkString)

  def string1(z: Int): Rng[String] =
    chars1(z) map (_.toList.mkString)

  def upperstring(z: Int): Rng[String] =
    uppers(z) map (_.mkString)

  def upperstring1(z: Int): Rng[String] =
    uppers1(z) map (_.toList.mkString)

  def lowerstring(z: Int): Rng[String] =
    lowers(z) map (_.mkString)

  def lowerstring1(z: Int): Rng[String] =
    lowers1(z) map (_.toList.mkString)

  def alphastring(z: Int): Rng[String] =
    alphas(z) map (_.mkString)

  def alphastring1(z: Int): Rng[String] =
    alphas1(z) map (_.toList.mkString)

  def numericstring(z: Int): Rng[String] =
    numerics(z) map (_.mkString)

  def numericstring1(z: Int): Rng[String] =
    numerics1(z) map (_.toList.mkString)

  def alphanumericstring(z: Int): Rng[String] =
    alphanumerics(z) map (_.mkString)

  def alphanumericstring1(z: Int): Rng[String] =
    alphanumerics1(z) map (_.toList.mkString)

  def identifier(z: Int): Rng[NonEmptyList[Char]] =
    for {
      a <- alpha
      b <- alphanumerics(if(z < 1) z + 1 else z - 1)
    } yield nel(a, b)

  def identifierstring(z: Int): Rng[String] =
    identifier(z) map (_.toList.mkString)

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

  def chooseDouble(l: Double, h: Double): Rng[Double] = {
    double map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      val diff = hh - ll
      if(diff == 0)
        ll
      else
        ll + math.abs(x * diff + ll)
    })
  }

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

  implicit def RngMonoid[A](implicit M: Monoid[A]): Monoid[Rng[A]] =
    new Monoid[Rng[A]] {
      def append(r1: Rng[A], r2: => Rng[A]) =
        r1 |+| r2

      def zero =
        insert(M.zero)
    }
}


object Main {
  import Rng._

  def main(args: Array[String]) {
    val r = uppers(1000)
    println(r.run)
  }
}