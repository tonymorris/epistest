package org.epistest

import scalaz._
                                         /*
sealed trait Corng[+A] {
  val cofree: Cofree[CorngOp, A]

  def map[B](f: A => B): Corng[B] =
    Corng(cofree map f)

  def coflatMap[B](f: Corng[A] => B): Corng[B] =
    Corng(cofree extend (r => f(Corng(r))))

  def duplicate: Corng[Corng[A]] =
    coflatMap(z => z)

  def extract: A =
    cofree.copure

  def inject[B](b: B): Corng[B] =
    Corng(cofree inject b)

  def scanr[B](g: (A, CorngOp[Corng[B]]) => B): Corng[B] =
    Corng(cofree scanr ((a, c: CorngOp[Cofree[CorngOp, B]]) => g(a, c map (Corng(_)))))

  def applyCofree[B](f: A => B, g: Corng[A] => Corng[B]): Corng[B] =
    Corng(cofree.applyCofree(f, q => g(Corng(q)).cofree))

  def applyTail[B](b: B, g: Corng[A] => Corng[B]): Corng[B] =
    Corng(cofree.applyTail(b, q => g(Corng(q)).cofree))

  def maph[G[+_]](f: CorngOp ~> G)(implicit G: Functor[G]): Cofree[G, A] =
    cofree mapBranching f

  def mapr(f: CorngOp ~> CorngOp): Corng[A] =
    Corng(cofree mapFirstBranching f)

}

object Corng {
  private[epistest] def apply[A](f: Cofree[CorngOp, A]): Corng[A] =
    new Corng[A] {
      val cofree = f
    }

  implicit val CorngComonad: Comonad[Corng] =
    new Comonad[Corng] {
      def map[A, B](a: Corng[A])(f: A => B) =
        a map f
      def cobind[A, B](a: Corng[A])(f: Corng[A] => B) =
        a coflatMap f
      def copoint[A](x: Corng[A]) =
        x.extract
      def cojoin[A](x: Corng[A]) =
        x.duplicate
    }

}
                                           */