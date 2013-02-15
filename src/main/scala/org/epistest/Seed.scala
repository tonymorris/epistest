package org.epistest

import scalaz._

sealed trait Seed {
  val option: Option[Long]

  import Seed._

  def orZero: Long =
    option getOrElse 0

  def get(x: => Long): Long =
    option getOrElse x

  def has: Boolean =
    option.isDefined

  def isDefault: Boolean =
    !has

  def |(s: => Seed): Seed =
    if (option.isDefined)
      this
    else
      s

  def withseed(f: Long => Long): Seed =
    option match {
      case None => defaultseed
      case Some(l) => Seed(f(l))
    }

  def +(l: => Long): Seed =
    withseed(_+l)

  def -(l: => Long): Seed =
    withseed(_-l)

  def inc: Seed =
    this + 1

  def dec: Seed =
    this + (-1)

  def forall(p: Long => Boolean): Boolean =
    option forall p

  def exists(p: Long => Boolean): Boolean =
    option exists p

}

object Seed {
  def apply(l: Long): Seed =
    new Seed {
      val option = Some(l)
    }

  def defaultseed: Seed =
    new Seed {
      val option = None
    }

  implicit val SeedMonoid: Monoid[Seed] =
    new Monoid[Seed] {
      def append(s1: Seed, s2: => Seed) =
        s1 | s2
      def zero =
        defaultseed
    }

  implicit val SeedOrder: Order[Seed] =
    implicitly[Order[Option[Long]]].contramap(_.option)
}
