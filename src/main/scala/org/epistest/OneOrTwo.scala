package org.epistest

import scalaz._, Scalaz._, NonEmptyList._

// todo move to scalaz
sealed trait OneOrTwo[+A] {
  val one: A
  val two: Option[A]

  import OneOrTwo.oneortwo

  def isOne: Boolean =
    two.isEmpty

  def isTwo: Boolean =
    !isOne

  def tryswap: OneOrTwo[A] =
    two match {
      case None => this
      case Some(a) => OneOrTwo.two(a, one)
    }

  def unary_~ : OneOrTwo[A] =
    tryswap

  def twoOr[AA >: A](a: => AA): AA =
    two getOrElse a

  def twoOrOne: A =
    twoOr(one)

  def toOne: OneOrTwo[A] =
    OneOrTwo(one)

  def setOne[AA >: A](a: AA): OneOrTwo[AA] =
    oneortwo(a, two)

  def setTwo[AA >: A](a: AA): OneOrTwo[AA] =
    optionTwo(Some(a))

  def unsetTwo: OneOrTwo[A] =
    optionTwo(None)

  def optionTwo[AA >: A](a: Option[AA]): OneOrTwo[AA] =
    oneortwo(one, a)

  def withOne[AA >: A](f: A => AA): OneOrTwo[AA] =
    oneortwo(f(one), two)

  def withTwo[AA >: A](f: A => AA): OneOrTwo[AA] =
    oneortwo(one, two map f)

  def withOptionTwo[AA >: A](f: Option[A] => Option[AA]): OneOrTwo[AA] =
    oneortwo(one, f(two))

  def list: List[A] =
    one :: two.toList

  def list1: NonEmptyList[A] =
    nel(one, two.toList)

  def vector: Vector[A] =
    Vector(list: _*)

  def stream[AA >: A]: EphemeralStream[AA] =
    EphemeralStream(list: _*)

  def combine: Option[(A, A)] =
    two map ((one, _))

  def pair: (A, Option[A]) =
    (one, two)

  def either: A \/ A =
    two match {
      case None => one.left
      case Some(t) => t.right
    }

  def map[B](f: A => B): OneOrTwo[B] =
    oneortwo(f(one), two map f)

  def ap[B](f: OneOrTwo[A => B]): OneOrTwo[B] =
    oneortwo(f.one(one), f.two flatMap(two map _))

  def zip[X](q: OneOrTwo[X]): OneOrTwo[(A, X)] =
    zipWith(q)(a => (a, _))

  def traverse[F[_], B](f: A => F[B])(implicit F: Apply[F]): F[OneOrTwo[B]] =
    two match {
      case None =>
        F.map(f(one))(OneOrTwo(_))
      case Some(t) =>
        F.apply2(f(one), f(t))(OneOrTwo.two(_, _))
    }

  def ***[X](q: OneOrTwo[X]): OneOrTwo[(A, X)] =
    zip(q)

  def zipWith[B, C](r: OneOrTwo[B])(f: A => B => C): OneOrTwo[C] =
    r.ap(map(f))

  def ++[AA >: A](x: OneOrTwo[AA])(implicit S: Semigroup[AA]): OneOrTwo[AA] =
    oneortwo(S.append(one, x.one), for {
      a <- two
      b <- x.two
    } yield S.append(a, b))

  def |+|[AA >: A](x: OneOrTwo[AA])(implicit S: Semigroup[AA]): OneOrTwo[AA] =
    oneortwo(S.append(one, x.one), two orElse x.two)

  def |||[AA >: A](x: => OneOrTwo[AA]): OneOrTwo[AA] =
    two match {
      case None => x
      case Some(_) => this
    }
}

object OneOrTwo extends OneOrTwoInstances {
  private[epistest] def oneortwo[A](o: A, t: Option[A]): OneOrTwo[A] =
    new OneOrTwo[A] {
      val one = o
      val two = t
    }

  def apply[A](a: A): OneOrTwo[A] =
    new OneOrTwo[A] {
      val one = a
      val two = None
    }

  def two[A](a1: A, a2: A): OneOrTwo[A] =
    new OneOrTwo[A] {
      val one = a1
      val two = Some(a2)
    }

  def unzip[A, B](x: OneOrTwo[(A, B)]): (OneOrTwo[A], OneOrTwo[B]) = {
    val ((a, b), z) = x.pair
    z match {
      case None =>
        (OneOrTwo(a), OneOrTwo(b))
      case Some((a2, b2)) =>
        (OneOrTwo.two(a, a2), OneOrTwo.two(b, b2))
    }
  }

  implicit val OneOrTwoZip: Zip[OneOrTwo] =
    new Zip[OneOrTwo] {
      def zip[A, B](a: => OneOrTwo[A], b: => OneOrTwo[B]) =
        a zip b
    }

  implicit val OneOrTwoUnzip: Unzip[OneOrTwo] =
    new Unzip[OneOrTwo] {
      def unzip[A, B](a: OneOrTwo[(A, B)]) =
        OneOrTwo.unzip(a)
    }
}

trait OneOrTwoInstances extends OneOrTwoInstances0 {
  implicit val OneOrTwoTraverse: Traverse[OneOrTwo] =
    new OneOrTwoTraverse{}

  implicit def OneOrTwoMonoid[A](implicit T: Monoid[A]): Monoid[OneOrTwo[A]] =
    new OneOrTwoMonoid[A] {
      implicit val S = T
      implicit val M = T
    }
}

trait OneOrTwoInstances0 {
  implicit val OneOrTwoApply: Apply[OneOrTwo] =
    new OneOrTwoApply{}

  implicit def OneOrTwoSemigroup[A](implicit T: Semigroup[A]): Semigroup[OneOrTwo[A]] =
    new OneOrTwoSemigroup[A] {
      implicit val S = T
    }
}

private[epistest] trait OneOrTwoApply extends Apply[OneOrTwo] {
  def ap[A, B](fa: => OneOrTwo[A])(f: => OneOrTwo[A => B]) =
    fa ap f
  def map[A, B](fa: OneOrTwo[A])(f: A => B) =
    fa map f
}

private[epistest] trait OneOrTwoTraverse extends Traverse[OneOrTwo] {
  def traverseImpl[F[_], A, B](fa: OneOrTwo[A])(f: A => F[B])(implicit A: Applicative[F]) =
    fa traverse f
}

private[epistest] trait OneOrTwoSemigroup[A] extends Semigroup[OneOrTwo[A]] {
  implicit val S: Semigroup[A]

  def append(a1: OneOrTwo[A], a2: => OneOrTwo[A]) =
    a1 |+| a2
}

private[epistest] trait OneOrTwoMonoid[A] extends Monoid[OneOrTwo[A]] with OneOrTwoSemigroup[A] {
  implicit val M: Monoid[A]

  def zero = OneOrTwo(M.zero)
}
