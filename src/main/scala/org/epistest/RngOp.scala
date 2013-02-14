package org.epistest

import scalaz._, Free._

sealed trait RngOp[+A] {
  val bits: Int
  val next: Int => A

  def map[B](f: A => B): RngOp[B] =
    RngOp(bits, f compose next)

  def lift: Rng[A] =
    Rng(Suspend(map(Return(_))))

  def coflatMap[B](f: RngOp[A] => B): RngOp[B] =
    RngOp(bits, w => f(RngOp(bits, next)))

  def duplicate: RngOp[RngOp[A]] =
    coflatMap(z => z)

  def extract: A =
    next(bits)

  def zap[B](x: CorngOp[B]): (A, B) =
    (x zap this).swap

  def zapWith[B, C](x: CorngOp[B])(f: A => B => C): C =
    x.zapWith(this)(b => f(_)(b))

  def store: Store[Int, A] =
     Store(next, bits)
}

object RngOp {
  def apply[A](b: Int, q: Int => A): RngOp[A] =
    new RngOp[A] {
      val bits = b
      val next = q
    }

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

  implicit val RngOpZap: Zap[RngOp, CorngOp] =
    new Zap[RngOp, CorngOp] {
      def zapWith[A, B, C](fa: RngOp[A], gb: CorngOp[B])(f: (A, B) => C) =
        fa.zapWith(gb)(f.curried)
    }
}
