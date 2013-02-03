package org.epistest

import scalaz._

sealed trait RngOp[+A] {
  def map[B](f: A => B): RngOp[B] =
    this match {
      case NextDouble(q) => NextDouble(f compose q)
      case NextLong(q) => NextLong(f compose q)
    }

  def runDouble(d: => Double): Option[A] =
    this match {
      case NextDouble(q) => Some(q(d))
      case NextLong(_) => None
    }

  def runLong(l: => Long): Option[A] =
    this match {
      case NextDouble(_) => None
      case NextLong(q) => Some(q(l))
    }

  def doubleK: Kleisli[Option, Double, A] =
    Kleisli(runDouble(_))

  def longK: Kleisli[Option, Long, A] =
    Kleisli(runLong(_))

  def run(d: => Double, l: => Long): A =
    this match {
      case NextDouble(q) => q(d)
      case NextLong(q) => q(l)
    }
}
private case class NextDouble[+A](q: Double => A) extends RngOp[A]
private case class NextLong[+A](q: Long => A) extends RngOp[A]

object RngOp {
  implicit val RngOpFunctor: Functor[RngOp] =
    new Functor[RngOp] {
      def map[A, B](a: RngOp[A])(f: A => B) =
        a map f
    }
}
