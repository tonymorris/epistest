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

  def coflatMap[B](f: RngOp[A] => B): RngOp[B] =
    this match {
      case NextBits(b, q) => {
        NextBits(b, w => f(NextBits(b, q)))
      }
    }

  def duplicate: RngOp[RngOp[A]] =
    coflatMap(z => z)

  def extract: A =
    this match {
      case NextBits(b, q) =>
        q(b)
    }

  def store: Store[Int, A] =
    this match {
      case NextBits(b, q) =>
        Store(q, b)
    }
}
private case class NextBits[+A](b: Int, q: Int => A) extends RngOp[A]

object RngOp {
  def apply[A](b: Int, q: Int => A): RngOp[A] =
    NextBits(b, q)

  def store[A](x: Store[Int, A]): RngOp[A] =
    NextBits(x.pos, x.put)

  implicit val RngOpComonad: Comonad[RngOp] =
    new Comonad[RngOp] {
      def map[A, B](a: RngOp[A])(f: A => B) =
        a map f
      def cobind[A, B](a: RngOp[A])(f: RngOp[A] => B) =
        a coflatMap f
      def copoint[A](x: RngOp[A]) =
        x.extract
      def cojoin[A](x: RngOp[A]) =
        x.duplicate
    }

  def demote[A, B](a: RngOp[A => B]): A => RngOp[B] =
    w =>
      a match {
        case NextBits(b, q) => NextBits(b, n => q(n)(w))
      }

}
