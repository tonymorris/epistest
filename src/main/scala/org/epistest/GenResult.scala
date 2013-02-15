package org.epistest

import scalaz._, Scalaz._

sealed trait GenResult[+A] {
  val size: Size
  val value: A

  def map[B](f: A => B): GenResult[B] =
    GenResult(size, f(value))

  def coflatMap[B](f: GenResult[A] => B): GenResult[B] =
    GenResult(size, f(this))

  def duplicate: GenResult[GenResult[A]] =
    GenResult(size, GenResult(size, value))

}

object GenResult {
  def apply[A](s: Size, v: A): GenResult[A] =
    new GenResult[A] {
      val size = s
      val value = v
    }

  def distribute[F[_], B](a: GenResult[F[B]])(implicit D: Distributive[F]): F[GenResult[B]] =
    D.cosequence(a)

  def distributeR[A, B](a: GenResult[A => B]): A => GenResult[B] =
    distribute[({type f[x] = A => x})#f, B](a)

  def distributeRK[A, B](a: GenResult[A => B]): Kleisli[GenResult, A, B] =
    Kleisli(distributeR(a))

  def distributeK[F[+_]: Distributive, A, B](a: GenResult[Kleisli[F, A, B]]): Kleisli[F, A, GenResult[B]] =
    distribute[({type f[x] = Kleisli[F, A, x]})#f, B](a)

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
