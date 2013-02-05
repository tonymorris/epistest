package org.epistest

import scalaz._, Free._

sealed trait RngOp[+A] {
  def map[B](f: A => B): RngOp[B] =
    this match {
      case NextDouble(q) => NextDouble(f compose q)
      case NextFloat(q) => NextFloat(f compose q)
      case NextLong(q) => NextLong(f compose q)
      case NextInt(q) => NextInt(f compose q)
    }

  def runDouble(d: => Double): Option[A] =
    this match {
      case NextDouble(q) => Some(q(d))
      case NextFloat(_) => None
      case NextLong(_) => None
      case NextInt(_) => None
    }

  def runFloat(d: => Float): Option[A] =
    this match {
      case NextDouble(_) => None
      case NextFloat(q) => Some(q(d))
      case NextLong(_) => None
      case NextInt(_) => None
    }

  def runLong(l: => Long): Option[A] =
    this match {
      case NextDouble(_) => None
      case NextFloat(_) => None
      case NextLong(q) => Some(q(l))
      case NextInt(_) => None
    }

  def runInt(i: => Int): Option[A] =
    this match {
      case NextDouble(_) => None
      case NextFloat(_) => None
      case NextLong(_) => None
      case NextInt(q) => Some(q(i))
    }

  def doubleK: Kleisli[Option, Double, A] =
    Kleisli(runDouble(_))

  def floatK: Kleisli[Option, Float, A] =
    Kleisli(runFloat(_))

  def longK: Kleisli[Option, Long, A] =
    Kleisli(runLong(_))

  def intK: Kleisli[Option, Int, A] =
    Kleisli(runInt(_))

  def lift: Rng[A] =
    Rng(Suspend(map(Return(_))))

  def run(d: => Double, f: => Float, l: => Long, i: => Int): A =
    this match {
      case NextDouble(q) => q(d)
      case NextFloat(q) => q(f)
      case NextLong(q) => q(l)
      case NextInt(q) => q(i)
    }
}
private case class NextDouble[+A](q: Double => A) extends RngOp[A]
private case class NextFloat[+A](q: Float => A) extends RngOp[A]
private case class NextLong[+A](q: Long => A) extends RngOp[A]
private case class NextInt[+A](q: Int => A) extends RngOp[A]

object RngOp {
  implicit val RngOpFunctor: Functor[RngOp] =
    new Functor[RngOp] {
      def map[A, B](a: RngOp[A])(f: A => B) =
        a map f
    }
}
