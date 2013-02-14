package org.epistest

import scalaz._, Free._

sealed trait RngOp[+A] {
  def map[B](f: A => B): RngOp[B] =
    this match {
      case NextBits(b, q) =>
        NextBits(b, f compose q)
    }

  def bits: Int =
    this match {
      case NextBits(b, _) =>
        b
    }

  def next: Int => A =
    this match {
      case NextBits(_, q) =>
        q
    }

  def lift: Rng[A] =
    Rng(Suspend(map(Return(_))))

  def store: Store[Int, A] =
    this match {
      case NextBits(b, q) =>
        Store(q, b)
    }
}
private case class NextBits[+A](b: Int, q: Int => A) extends RngOp[A]

object RngOp {
  implicit val RngOpFunctor: Functor[RngOp] =
    new Functor[RngOp] {
      def map[A, B](a: RngOp[A])(f: A => B) =
        a map f
    }

  def demote[A, B](a: RngOp[A => B]): A => RngOp[B] =
    w =>
      a match {
        case NextBits(b, q) => NextBits(b, n => q(n)(w))
      }

}
