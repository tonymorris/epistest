package org.epistest

import scalaz._, Free._

sealed trait RngOp[+A] {
  def map[B](f: A => B): RngOp[B] =
    this match {
      case NextDouble(q) => NextDouble(f compose q)
      case NextLong(q) => NextLong(f compose q)
      case NextInt(q) => NextInt(f compose q)
    }


  def lift: Rng[A] =
    Rng(Suspend(map(Return(_))))
}
private case class NextDouble[+A](q: Double => A) extends RngOp[A]
private case class NextLong[+A](q: Long => A) extends RngOp[A]
private case class NextInt[+A](q: Int => A) extends RngOp[A]

object RngOp {
  implicit val RngOpFunctor: Functor[RngOp] =
    new Functor[RngOp] {
      def map[A, B](a: RngOp[A])(f: A => B) =
        a map f
    }
}
