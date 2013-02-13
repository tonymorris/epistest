package org.epistest

import scalaz._, Scalaz._, NonEmptyList._

// todo move to scalaz
sealed trait MaybeEmptyList[+A] { outer =>
  val maybeHead: Option[A]
  val rest: List[A]

  def list: List[A] =
    maybeHead match {
      case None => rest
      case Some(a) => a::rest
    }

  def either: List[A] \/ NonEmptyList[A] =
    maybeHead match {
      case None => rest.left
      case Some(a) => nel(a, rest).right
    }

  def head: Option[A] =
    maybeHead orElse rest.headOption

  def ::[AA >: A](a: AA): MaybeEmptyList[AA] =
    MaybeEmptyList.nonEmpty(nel(a, list))

  def ++[AA >: A](a: MaybeEmptyList[AA]) =
    new MaybeEmptyList[AA] {
      val maybeHead = outer.maybeHead
      val rest = outer.rest ::: a.list
    }

  def foldRight[B](z: B)(f: (A, B) => B): B =
    list.foldRight(z)(f)

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    list.foldLeft(z)(f)

  def map[B](f: A => B): MaybeEmptyList[B] =
    maybeHead match {
      case None => MaybeEmptyList.list(rest map f)
      case Some(a) => MaybeEmptyList.nonEmpty(nel(f(a), rest map f))
    }

  def flatMap[B](f: A => MaybeEmptyList[B]): MaybeEmptyList[B] =
    foldRight(MaybeEmptyList.empty[B])(f(_) ++ _)
}

object MaybeEmptyList {
  def empty[A]: MaybeEmptyList[A] =
    list(Nil)

  def single[A](a: A): MaybeEmptyList[A] =
    list(List(a))

  def list[A](x: List[A]): MaybeEmptyList[A] =
    new MaybeEmptyList[A] {
      val maybeHead = None
      val rest = x
    }

  def nonEmpty[A](x: NonEmptyList[A]): MaybeEmptyList[A] =
    new MaybeEmptyList[A] {
      val maybeHead = Some(x.head)
      val rest = x.tail
    }

  def apply[A](as: A*): MaybeEmptyList[A] =
    as.toList match {
      case Nil => list(Nil)
      case h::t => nonEmpty(nel(h, t))
    }

  implicit val MaybeEmptyListMonad: Monad[MaybeEmptyList] =
    new Monad[MaybeEmptyList] {
      def bind[A, B](a: MaybeEmptyList[A])(f: A => MaybeEmptyList[B]) =
        a flatMap f
      def point[A](a: => A) =
        single(a)
    }

  implicit def MaybeEmptyListSemigroup[A]: Semigroup[MaybeEmptyList[A]] =
    new Semigroup[MaybeEmptyList[A]] {
      def append(r1: MaybeEmptyList[A], r2: => MaybeEmptyList[A]) =
        r1 ++ r2
    }

  implicit def MaybeEmptyListMonoid[A]: Monoid[MaybeEmptyList[A]] =
    new Monoid[MaybeEmptyList[A]] {
      def append(r1: MaybeEmptyList[A], r2: => MaybeEmptyList[A]) =
        r1 ++ r2

      def zero =
        empty
    }
}
