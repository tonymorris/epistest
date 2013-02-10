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

      val x = EphemeralStream.unfold(n, (o: Int) => if(o > 0) Some(o, o / 2) else None)
      val xx = x.flatMap(removes(_, n, l))
      val xxx = xx ++ shrink1(l)
      xxx
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

  def predecessors[A](implicit E: Enum[A]): Shrink[A] =
    Shrink(a => EphemeralStream.unfold(a, (q: A) =>
      if(E.min exists (_ === q))
        None
      else {
        val p = E.pred(q)
        Some((p, p))
      })
    )

  def predecessorsN[A](n: Int)(implicit E: Enum[A]): Shrink[A] =
    Shrink(a => EphemeralStream.unfold(a, (q: A) =>
      if(E.min exists (_ === q))
        None
      else {
        val p = E.predn(n, q)
        Some((p, p))
      })
    )

  def successors[A](implicit E: Enum[A]): Shrink[A] =
    Shrink(a => EphemeralStream.unfold(a, (q: A) =>
      if(E.max exists (_ === q))
        None
      else {
        val p = E.succ(q)
        Some((p, p))
      })
    )

  def successorsN[A](n: Int)(implicit E: Enum[A]): Shrink[A] =
    Shrink(a => EphemeralStream.unfold(a, (q: A) =>
      if(E.max exists (_ === q))
        None
      else {
        val p = E.succn(n, q)
        Some((p, p))
      })
    )

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
            if(x == 0)
              None
            else {
              val r = n - x / 2
              Some(((r, -r), x / 2))
            }).unzip[Int, Int]

        cons(0, interleave(halfs, nhalfs))
      })
}

object Main {
  def main(args: Array[String]) {
    val g = Shrink.predecessors[Int]
    val r = g(Int.MinValue + 100)
    println(r.toList)
  }
}
