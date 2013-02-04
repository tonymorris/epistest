package org.epistest

import scalaz._, Free._, Scalaz._

sealed trait Rng[+A] {
  val free: Free[RngOp, A]

  def map[B](f: A => B): Rng[B] =
    Rng(free map f)

  def flatMap[B](f: A => Rng[B]): Rng[B] =
    Rng(free flatMap (f(_).free))

  def ap[X](f: Rng[A => X]): Rng[X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: Rng[X]): Rng[(A, X)] =
    for {
      a <- this
      x <- q
    } yield (a, x)

  def zap[G[+_], B](fs: Cofree[G, A => B])(implicit G: Functor[G], d: Zap[RngOp, G]): B =
    free.zap(fs)

  def foldRun[B, AA >: A](b: B)(f: (B, RngOp[Rng[AA]]) => (B, Rng[AA])): (B, AA) =
    free.foldRun[B, AA](b)((bb, t) => f(bb, t map (Rng(_))) :-> (_.free))

  def resume: RngResume[A] =
    free.resume match {
      case -\/(x) => RngCont(x map (Rng(_)))
      case \/-(x) => RngTerm(x)
    }

  // CAUTION: unsafe
  final def run: A = {
    @annotation.tailrec
    def loop(g: Rng[A], r: java.util.Random): A =
      resume match {
        case RngCont(NextDouble(q)) =>
          loop(q(r.nextDouble), r)
        case RngCont(NextLong(q)) =>
          loop(q(r.nextLong), r)
        case RngTerm(a) =>
          a
      }

    loop(this, new java.util.Random)
  }

  def maph[G[+_]](f: RngOp ~> G)(implicit G: Functor[G]): Free[G, A] =
    resume match {
      case RngCont(q) =>
        Suspend(f(q map (r => Rng(r.free) maph f)))
      case RngTerm(a) =>
        Return(a)
    }

  def mapr(f: RngOp ~> RngOp): Rng[A] =
    Rng(resume match {
      case RngCont(q) =>
        Suspend(f(q map (_.free)))
      case RngTerm(a) =>
        Return(a)
    })

  def go[AA >: A](f: RngOp[Rng[AA]] => Rng[AA]): AA =
    free.go[AA](r => f(r map (Rng(_))).free)
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

  def insert[A](a: A): Rng[A] =
    Rng(Return(a))
}
