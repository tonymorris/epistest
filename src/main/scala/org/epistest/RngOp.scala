package org.epistest

import scalaz._, Free._

sealed trait RngOp[+A] {
  def map[B](f: A => B): RngOp[B] =
    this match {
      case NextBits(b, n) => RngOp(b, f compose n)
    }

  def lift: Rng[A] =
    Rng(Suspend(map(Return(_))))

  def coflatMap[B](f: RngOp[A] => B): RngOp[B] =
    this match {
      case NextBits(b, n) => RngOp(b, w => f(RngOp(b, n)))
    }

  def duplicate: RngOp[RngOp[A]] =
    coflatMap(z => z)

  def extract: A =
    this match {
      case NextBits(b, n) => n(b)
    }
/*
  def zap[B](x: CorngOp[B]): (A, B) =
    (x zap this).swap

  def zapWith[B, C](x: CorngOp[B])(f: A => B => C): C =
    x.zapWith(this)(b => f(_)(b))
  */
  def store: Store[Int, A] =
    this match {
      case NextBits(b, n) => Store(n, b)
    }
}
private case class NextBits[+A](b: Int, n: Int => A) extends RngOp[A]

object RngOp {
  def apply[A](b: Int, n: Int => A): RngOp[A] =
    NextBits(b, n)

  def store[A](x: Store[Int, A]): RngOp[A] =
    apply(x.pos, x.put)

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
                        /*
  implicit val RngOpZap: Zap[RngOp, CorngOp] =
    new Zap[RngOp, CorngOp] {
      def zapWith[A, B, C](fa: RngOp[A], gb: CorngOp[B])(f: (A, B) => C) =
        fa.zapWith(gb)(f.curried)
    }
    */
}
