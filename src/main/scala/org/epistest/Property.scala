package org.epistest

import scalaz._, Scalaz._

sealed trait Result[+A]
case class Exhausted[+A]() extends Result[A]
case class Succeed[+A](succeed: List[A]) extends Result[A]
case class Failed[+A](succeed: List[A], shrinksucceed: List[A], failed: A \/ (A, A)) extends Result[A]


// case class PropertyWithDiscarded[-A](run: A => (Interval, Boolean))
case class Property[-A](run: A => Boolean) {
  def check[AA <: A](g: Gen[AA], sh: Shrink[AA], ex: Interval[AA], tests: Int, sz: Size)(implicit E: Enum[AA]): Result[AA] = {
    val o = Diev.empty[AA] + ((ex.min, ex.max))
    val y = g fill tests map (l => {
      def f(x: List[AA], d: Diev[AA], r: Result[AA]): Result[AA] =
        x match {
          case Nil => r
          case h::t => {
            if(run(h)) {
              val e = d + h
              if(e === o)
                Exhausted()
              else
                f(t, e, r match {
                  case Exhausted() => Exhausted()
                  case Succeed(s) => Succeed(h::s)
                  case Failed(s, i, f) => Failed(s, i, f)
                })
            } else {
              def g(x: EphemeralStream[AA], v: (List[AA], Option[AA])): (List[AA], Option[AA]) =
                if(x.isEmpty)
                  v
                else {
                  val hh = x.head()
                  v match {
                    case (p, qq) => {
                      val d = run(hh)
                      if(d)
                        g(x.tail(), (hh :: p, qq))
                      else {
                        if(qq exists (_ == hh))
                          (p, Some(hh))
                        else
                          g(sh(hh), (p, Some(hh)))
                      }
                    }
                  }
                }

              r match {
                case Exhausted() => Exhausted()
                case Succeed(s) => {
                  val (e, z) = g(sh(h), (Nil, None))
                  Failed(s, e, z match {
                    case None => h.left
                    case Some(u) => (h, u).right
                  })
                }
                case Failed(s, i, f) => Failed(s, i, f)
              }
            }
          }
        }
      f(l, Diev.empty, Succeed(Nil))
    })

    y.run(sz)
  }
}

object Property {
  val plusZero = Property((n: Int) => n + 0 == n)
  val plusZeroBreaks = Property((n: Int) => if(n > 24 && n < 100000000) n == 77 else n + 0 == n)

}

object Main {
  def main(args: Array[String]) {
    val p = Property.plusZeroBreaks
    val r = p.check(Gen.int, Shrink.int, Interval.Range.int, 100, Size.nosize)
    println(r)
  }
}