package org.epistest

import scalaz._, Scalaz._

sealed trait CorngOp[+A] {
  val double: Double
  val float: Float
  val int: Int
  val long: Long
  val value: A
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
  implicit val CorngOpFunctor: Functor[CorngOp] =
    new Functor[CorngOp] {
      def map[A, B](a: CorngOp[A])(f: A => B) =
        a map f
    }
}