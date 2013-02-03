package org.epistest

import scalaz._, Free._

sealed trait Rng[+A] {
  val free: Free[RngOp, A]

  def map[B](f: A => B): Rng[B] =
    Rng(free map f)

  def flatMap[B](f: A => Rng[B]): Rng[B] =
    Rng(free flatMap (f(_).free))

  def resume: RngResume[A] =
    free.resume match {
      case -\/(x) => RngCont(x map (Rng(_)))
      case \/-(x) => RngTerm(x)
    }

  @annotation.tailrec
  final def run(r: java.util.Random): A =
    resume match {
      case RngCont(NextDouble(q)) =>
        q(r.nextDouble) run r
      case RngCont(NextLong(q)) =>
        q(r.nextLong) run r
      case RngTerm(a) =>
        a
    }
}

object Rng {
  private[epistest] def apply[A](f: Free[RngOp, A]): Rng[A] =
    new Rng[A] {
      val free = f
    }

  def nextDouble: Rng[Double] =
    Rng(Suspend(NextDouble(Return(_))))

  def nextLong: Rng[Long] =
    Rng(Suspend(NextLong(Return(_))))
}