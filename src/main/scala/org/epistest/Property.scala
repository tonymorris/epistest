package org.epistest

import scalaz._, Scalaz._

sealed trait Property[A, R] {
  val run: A => R

  def contramap[B](f: B => A): Property[B, R] =
    Property(run compose f)

  def map[S](f: R => S): Property[A, S] =
    Property(f compose run)

  def flatMap[S](f: R => Property[A, S]): Property[A, S] =
    Property(a => f(run(a)) run a)

  def ap[X](f: Property[A, R => X]): Property[A, X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: Property[A, X]): Property[A, (R, X)] =
    zipWith(q)(a => (a, _))

  def zipWith[B, C](r: Property[A, B])(f: R => B => C): Property[A, C] =
    r.ap(map(f))

  def compose[S](p: Property[S, A]): Property[S, R] =
    Property(run compose p.run)

  def andThen[S](p: Property[R, S]): Property[A, S] =
    p compose this

  def ***[B, S](p: Property[B, S]): Property[(A, B), (R, S)] =
    Property {
      case (a, b) => (run(a), p.run(b))
    }

  def +++[B, S](p: Property[B, S]): Property[A \/ B, R \/ S] =
    Property {
      case -\/(a) => -\/(run(a))
      case \/-(b) => \/-(p.run(b))
    }

  def |||[B](p: Property[B, R]): Property[A \/ B, R] =
    Property {
      case -\/(a) => run(a)
      case \/-(b) => p.run(b)
    }

  def check(tests: Int, sz: Size = Size.nosize)(implicit D: Decision[R], T: DataGenShrink[A]): Result[A] = {
    val w =
      T.gen fill tests map (l => {
        @annotation.tailrec
        def successloop(x: List[A], r: Result[A]): Result[A] =
          x match {
            case Nil =>
              r
            case h::t =>
              if(D(run(h)))
                successloop(t, r.withSucceed(h::_))
              else {
                @annotation.tailrec
                def failedloop(y: EphemeralStream[A], v: (List[A], Option[A])): (List[A], Option[A]) =
                  if(y.isEmpty)
                    v
                  else {
                    val (z, o) = v
                    val hh = y.head()
                    if(D(run(hh)))
                      failedloop(y.tail(), v)
                    else if (o exists (_ == hh))
                      (z, Some(hh))
                    else
                      failedloop(T.shrink(hh), (z, Some(hh)))

                  }

                r match {
                  case Succeed(s) => {
                    val (e, z) = failedloop(T.shrink(h), (Nil, None))
                    Failed(s, e, OneOrTwo(h) optionTwo z)
                  }
                  case Failed(s, k, i) => Failed(s, k, i)
                }
              }
          }

        successloop(l, Succeed(Nil))
      })

    w run sz
  }

}

sealed trait DataGenShrink[A] {
  val gen: Gen[A]
  val shrink: Shrink[A]

  def zip[B](b: DataGenShrink[B]): DataGenShrink[(A, B)] =
    DataGenShrink(gen zip b.gen, shrink zip b.shrink)

}

object DataGenShrink {
  def apply[A](g: Gen[A], s: Shrink[A]): DataGenShrink[A] =
    new DataGenShrink[A] {
      val gen = g
      val shrink = s
    }

  implicit val IntDataGenShrink: DataGenShrink[Int] =
    apply(Gen.int, Shrink.int)

  implicit val StringDataGenShrink: DataGenShrink[String] =
    apply(Gen.string, Shrink.string)

  implicit def Tuple2DataGenShrink[A, B](implicit A: DataGenShrink[A], B: DataGenShrink[B]): DataGenShrink[(A, B)] =
    A zip B
}

sealed trait Result[+A] {
  def withSucceed[AA >: A](f: List[A] => List[AA]): Result[AA] =
    this match {
      case Succeed(s) => Succeed(f(s))
      case Failed(s, h, i) => Failed(s, h, i)
    }
}
case class Succeed[+A](succeed: List[A]) extends Result[A]
case class Failed[+A](succeed: List[A], shrinksucceed: List[A], failed: OneOrTwo[A]) extends Result[A]


object Property {
  def apply[A, R](r: A => R): Property[A, R] =
    new Property[A, R] {
      val run = r
    }

  def _2[A, B, R](r: (A, B) => R): Property[(A, B), R] =
    apply(r.tupled)
}

object Main {
  val p1 = Property._2((x: Int, y: Int) => x + y == y + x)
  def main(args: Array[String]) {
    val r = p1.check(100, Size(5))
    println(r)
  }
}
