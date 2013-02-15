package org.epistest

import scalaz._

sealed trait GenResult[+A] {
  val size: Size
  val seed: Seed
  val value: A

  def map[B](f: A => B): GenResult[B] =
    GenResult(size, seed, f(value))

  def coflatMap[B](f: GenResult[A] => B): GenResult[B] =
    GenResult(size, seed, f(this))

  def duplicate: GenResult[GenResult[A]] =
    GenResult(size, seed, GenResult(size, seed, value))

}

object GenResult {
  def apply[A](s: Size, t: Seed, v: A): GenResult[A] =
    new GenResult[A] {
      val size = s
      val seed = t
      val value = v
    }

  def distribute[A, B](a: GenResult[A => B]): A => GenResult[B] =
    w => a map (_(w))

  implicit val GenResultComonad: Comonad[GenResult] =
    new Comonad[GenResult] {
      def map[A, B](a: GenResult[A])(f: A => B) =
        a map f
      def cobind[A, B](a: GenResult[A])(f: GenResult[A] => B) =
        a coflatMap f
      def cojoin[A](x: GenResult[A]) =
        x.duplicate
      def copoint[A](x: GenResult[A]) =
        x.value
    }

}
