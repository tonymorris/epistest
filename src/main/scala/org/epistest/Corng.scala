package org.epistest

import scalaz._, Scalaz._

sealed trait Corng[+A] {
  val cofree: Cofree[CorngOp, A]

  def map[B](f: A => B): Corng[B] =
    Corng(cofree map f)

  def zap[B](r: Rng[A => B]): B =
    cofree zap r.free

  def zapWith[B, C](r: Rng[B])(f: (A, B) => C): C =
    cofree.zapWith(r.free)(f)

  def coflatMap[B](f: Corng[A] => B): Corng[B] =
    Corng(cofree extend (c => f(Corng(c))))

  def duplicate: Corng[Corng[A]] =
    coflatMap(q => q)

  def extract: A =
    cofree.head

  def scanr[B](f: (A, CorngOp[Corng[B]]) => B): Corng[B] =
    Corng(cofree scanr ((a, r: CorngOp[Cofree[CorngOp, B]]) => f(a, r map (Corng(_)))))

  def inject[B](b: B): Corng[B] =
    Corng(cofree inject b)
}

object Corng {
  private[epistest] def apply[A](f: Cofree[CorngOp, A]): Corng[A] =
    new Corng[A] {
      val cofree = f
    }

  implicit val CorngComonad: Comonad[Corng] =
    new Comonad[Corng] {
      def cobind[A, B](a: Corng[A])(f: Corng[A] => B) =
        a coflatMap f
      def cojoin[A](a: Corng[A]) =
        a.coflatMap(z => z)
      def map[A, B](a: Corng[A])(f: A => B) =
        a map f
      def copoint[A](a: Corng[A]) =
        a.extract
    }

  implicit val CorngZap: Zap[Corng, Rng] =
    new Zap[Corng, Rng] {
      override def zapWith[A, B, C](c: Corng[A], r: Rng[B])(f: (A, B) => C) =
        c.zapWith(r)(f)
    }
}
