package org.epistest

import scalaz._, Free._

sealed trait RngOp[+A] {
  def map[B](f: A => B): RngOp[B] =
    this match {
      case NextBits(b, n) => NextBits(b, f compose n)
      case SetSeed(b, n) => SetSeed(b, () => f(n()))
    }

  def lift: Rng[A] =
    Rng(Suspend(map(Return(_))))

  def coflatMap[B](f: RngOp[A] => B): RngOp[B] =
    this match {
      case NextBits(b, n) => NextBits(b, w => f(NextBits(w, n)))
      case s@SetSeed(b, n) => SetSeed(b, () => f(s))
    }

  def duplicate: RngOp[RngOp[A]] =
    coflatMap(z => z)

  def extract: A =
    this match {
      case NextBits(b, n) => n(b)
      case SetSeed(_, n) => n()
    }

  def store: Option[Store[Int, A]] =
    this match {
      case NextBits(b, n) => Some(Store(n, b))
      case SetSeed(_, _) => None
    }
}
private case class NextBits[+A](b: Int, n: Int => A) extends RngOp[A]
private case class SetSeed[+A](b: Long, n: () => A) extends RngOp[A]

object RngOp {
  def nextbits[A](b: Int, n: Int => A): RngOp[A] =
    NextBits(b, n)

  def setseed[A](b: Long, a: => A): RngOp[A] =
    SetSeed(b, () => a)

  def store[A](x: Store[Int, A]): RngOp[A] =
    nextbits(x.pos, x.put)

  def distribute[A, B](a: RngOp[A => B]): A => RngOp[B] =
    w => a map (_(w))

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
}
