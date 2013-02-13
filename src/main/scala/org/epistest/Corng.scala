package org.epistest

import scalaz._, Scalaz._

sealed trait Corng[+A] {
  val cofree: Cofree[CorngOp, A]

  def map[B](f: A => B): Corng[B] =
    Corng(cofree map f)

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
}
