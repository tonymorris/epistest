package org.epistest

import scalaz._, Scalaz._

sealed trait CorngOp[+A] {
  val double: Double
  val float: Float
  val int: Int
  val long: Long
  val value: A

  def map[B](f: A => B): CorngOp[B] =
    CorngOp(double, float, int, long, f(value))

  def zap[B](r: RngOp[A => B]): B =
    r.zapWith(this)(_(_))

  def zapWith[B, C](r: RngOp[B])(f: (A, B) => C): C =
    f(value, r match {
      case NextDouble(q) =>
	      q(double)
      case NextFloat(q) =>
	      q(float)
      case NextLong(q) =>
	      q(long)
      case NextInt(q) =>
	      q(int)
    })
}

object CorngOp {
  private[epistest] def apply[A](d: Double, f: Float, i: Int, l: Long, v: A): CorngOp[A] =
    new CorngOp[A] {
      val double = d
      val float = f
      val int = i
      val long = l
      val value = v
    }

  implicit val CorngOpZap: Zap[CorngOp, RngOp] =
    new Zap[CorngOp, RngOp] {
      override def zapWith[A, B, C](c: CorngOp[A], r: RngOp[B])(f: (A, B) => C) =
        c.zapWith(r)(f)
    }

  implicit val CorngOpFunctor: Functor[CorngOp] =
    new Functor[CorngOp] {
      def map[A, B](a: CorngOp[A])(f: A => B) =
        a map f
    }

}