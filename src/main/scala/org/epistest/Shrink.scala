package org.epistest

import scalaz._, Scalaz._, BijectionT._, NonEmptyList._
import EphemeralStream.{cons, unfold}

sealed trait Shrink[A] {
  val shrink: A => EphemeralStream[A]

  def apply(a: A): EphemeralStream[A] =
    shrink(a)

  def andThen(s: Shrink[A]): Shrink[A] =
    Shrink(a => shrink(a) flatMap (s shrink _))

  def >=>(s: Shrink[A]): Shrink[A] =
    andThen(s)

  def xmap[B](f: A => B, g: B => A): Shrink[B] =
    Shrink(b => shrink(g(b)) map f)

  def biject[B](b: Bijection[A, B]): Shrink[B] =
    xmap(b.to, b.from)

  def zip[B](s: Shrink[B]): Shrink[(A, B)] =
    Shrink {
      case (a, b) => shrink(a).map((_, b)) ++ s.shrink(b).map((a, _))
    }

  def ***[B](s: Shrink[B]): Shrink[(A, B)] =
    zip(s)

  def option: Shrink[Option[A]] =
    Shrink {
      case None => EphemeralStream()
      case Some(q) => EphemeralStream.cons(None, shrink(q) map (Some(_)))
    }

  def either[B](s: Shrink[B]): Shrink[A \/ B] =
    Shrink {
      case -\/(a) => shrink(a) map (-\/(_))
      case \/-(b) => s.shrink(b) map (\/-(_))
    }

  def list: Shrink[List[A]] =
    Shrink(l => {
      val n = l.length
      def removes(k: Int, n: Int, x: List[A]): EphemeralStream[List[A]] =
        if (k > n)
          EphemeralStream()
        else {
          val (t, u) = x.splitAt(k)
          if (t.isEmpty)
            EphemeralStream(Nil)
          else
            EphemeralStream.cons(u, removes(k, n - k, u) map (t ::: _))
        }

      def shrink1(x: List[A]): EphemeralStream[List[A]] =
        x match {
          case Nil => EphemeralStream()
          case h::t => shrink(h) map (_ :: t)
        }

      val u = EphemeralStream.unfold(n, (o: Int) => if(o > 0) Some(o, o / 2) else None)
      u.flatMap(removes(_, n, l)) ++ shrink1(l)
    })

  def list1: Shrink[NonEmptyList[A]] =
    this zip list xmap (ht => nel(ht._1, ht._2), e => (e.head, e.tail))

  def vector: Shrink[Vector[A]] =
    list xmap (Vector(_: _*), _.toList)

  def stream: Shrink[EphemeralStream[A]] =
    list xmap (EphemeralStream(_: _*), _.toList)

}

object Shrink {
  def apply[A](f: A => EphemeralStream[A]): Shrink[A] =
    new Shrink[A] {
      val shrink = f
    }

  def empty[A]: Shrink[A] =
    Shrink(_ => EphemeralStream())

  def single[A](f: A => A): Shrink[A] =
    Shrink(EphemeralStream(_))

  def insert[A](a: => A): Shrink[A] =
    single(_ => a)

  // todo use the interleave method in scalaz-7.0.0-M8
  private def interleave[T](x: EphemeralStream[T], y: EphemeralStream[T]): EphemeralStream[T] =
    if(x.isEmpty) y
    else if(y.isEmpty) x
    else cons(x.head(), cons(y.head(), interleave(x.tail(), y.tail())))

  def unit: Shrink[Unit] =
    insert(())

  val boolean: Shrink[Boolean] =
    Shrink(p => if (p) EphemeralStream(false) else EphemeralStream())

  val ordering: Shrink[scalaz.Ordering] =
    Shrink {
      case Ordering.GT => EphemeralStream(Ordering.EQ, Ordering.LT)
      case Ordering.LT => EphemeralStream(Ordering.EQ)
      case Ordering.EQ => EphemeralStream()
    }

  val int: Shrink[Int] =
    Shrink(n =>
      if(n == 0)
        EphemeralStream()
      else {
        val (halfs, nhalfs) =
          unfold(n, (x: Int) =>
            if(x <= 1)
              None
            else {
              val r = n - x / 2
              Some(((r, -r), x / 2))
            }).unzip[Int, Int]

        cons(0, interleave(halfs, nhalfs))
      })

  val byte: Shrink[Byte] =
    Shrink(n =>
      if(n == 0)
        EphemeralStream()
      else {
        val (halfs, nhalfs) =
          unfold(n, (x: Byte) =>
            if(x <= 1)
              None
            else {
              val r = n - x / 2
              Some(((r.toByte, (-r).toByte), (x / 2).toByte))
            }).unzip[Byte, Byte]

        cons(0, interleave(halfs, nhalfs))
      })

  val short: Shrink[Short] =
    Shrink(n =>
      if(n == 0)
        EphemeralStream()
      else {
        val (halfs, nhalfs) =
          unfold(n, (x: Short) =>
            if(x <= 1)
              None
            else {
              val r = n - x / 2
              Some(((r.toShort, (-r).toShort), (x / 2).toShort))
            }).unzip[Short, Short]

        cons(0, interleave(halfs, nhalfs))
      })

  val long: Shrink[Long] =
    Shrink(n =>
      if(n == 0L)
        EphemeralStream()
      else {
        val (halfs, nhalfs) =
          unfold(n, (x: Long) =>
            if(x <= 1L)
              None
            else {
              val r = n - x / 2L
              Some(((r, -r), x / 2L))
            }).unzip[Long, Long]

        cons(0L, interleave(halfs, nhalfs))
      })

  val float: Shrink[Float] =
    Shrink(n =>
      if(n == 0F)
        EphemeralStream()
      else {
        val (halfs, nhalfs) =
          unfold(n, (x: Float) =>
            if(x <= 1F)
              None
            else {
              val r = n - x / 2F
              Some(((r, -r), x / 2F))
            }).unzip[Float, Float]

        cons(0F, interleave(halfs, nhalfs))
      })

  val double: Shrink[Double] =
    Shrink(n =>
      if(n == 0D)
        EphemeralStream()
      else {
        val (halfs, nhalfs) =
          unfold(n, (x: Double) =>
            if(x <= 1D)
              None
            else {
              val r = n - x / 2D
              Some(((r, -r), x / 2D))
            }).unzip[Double, Double]

        cons(0D, interleave(halfs, nhalfs))
      })

  val char: Shrink[Char] =
    Shrink(c => {
      def lt(a: Char): Boolean = {
          def stamp(x: Char) =
            (
              !x.isLower
            , !x.isUpper
            , !x.isDigit
            , x != ' '
            , !x.isWhitespace
            , x
            )
          stamp(a) < stamp(c)
        }

      val l =
        List(
          "abc".toList
        , if(c.isUpper) List(c.toLower) else Nil
        , "ABC".toList
        , "123".toList
        , " \n".toList
        ).flatten.distinct filter lt
      EphemeralStream(l: _*)
    })

  val digit: Shrink[Digit] =
    Shrink(d =>
      EphemeralStream.unfold(Digit._0, (q: Digit) => if(q == Digit._9 || q == d) None else {
        val n = implicitly[Enum[Digit]].succ(q)
        Some((q, n))
      })
    )

  val string: Shrink[String] =
    char.list xmap (_.mkString, _.toList)

}

sealed trait Result[+A]
case class Exhausted[+A]() extends Result[A]
case class Run[+A](succeed: List[A], failure: List[A]) extends Result[A]


// case class PropertyWithDiscarded[-A](run: A => (Interval, Boolean))
case class Property[-A](run: A => Boolean) {
  def check[AA <: A](g: Gen[AA], sh: Shrink[AA], ex: Interval[AA], tests: Int, sz: Size): Result[AA] = {
    val x = g fill tests
    val y = x map (l => {
      def f(r: List[AA]): Result[AA] =
        error("")
      f(l)
    })
    error("")
  }
}

object Property {
  val plusZero = Property((n: Int) => n + 0 == n)
}