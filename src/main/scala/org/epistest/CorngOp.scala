package org.epistest

import scalaz._, Leibniz._
             /*
sealed trait CorngOp[+A] {
  val run: Int => (A, Int)

  def apply(n: Int): (A, Int) =
    run(n)

  def value(n: Int): A =
    run(n)._1

  def nextbits(n: Int): Int =
    run(n)._2

  def map[B](f: A => B): CorngOp[B] =
    CorngOp(n => {
      val (a, o) = run(n)
      (f(a), o)
    })

  def flatMap[B](f: A => CorngOp[B]): CorngOp[B] =
    CorngOp(n => {
      val (a, o) = run(n)
      f(a) run o
    })

  def ap[X](f: CorngOp[A => X]): CorngOp[X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: CorngOp[X]): CorngOp[(A, X)] =
    zipWith(q)(a => (a, _))

  def zipWith[B, C](r: CorngOp[B])(f: A => B => C): CorngOp[C] =
    r.ap(map(f))

  def zap[B](x: RngOp[B]): (A, B) =
    zapWith(x)(a => (a, _))

  def zapWith[B, C](x: RngOp[B])(f: A => B => C): C = {
    val (a, o) = run(x.nextbits)
    f(a)(x next o)
  }

  def |+|[AA >: A](x: CorngOp[AA])(implicit S: Semigroup[AA]): CorngOp[AA] =
    for {
      a <- this
      b <- x
    } yield S.append(a, b)

  def ***[X](x: CorngOp[X]): CorngOp[(A, X)] =
    zip(x)

  def flatten[AA >: A, B](implicit f: AA === CorngOp[B]): CorngOp[B] =
    flatMap(f)

  def withbits(f: Int => Int): CorngOp[A] =
    CorngOp(n => {
      val (a, o) = run(n)
      (a, f(o))
    })

  def inc: CorngOp[A] =
    withbits(_ + 1)

  def dec: CorngOp[A] =
    withbits(_ - 1)
}

object CorngOp {
  def apply[A](f: Int => (A, Int)): CorngOp[A] =
    new CorngOp[A] {
      val run = f
    }

  def state[A](x: State[Int, A]): CorngOp[A] =
    apply(n => x(n).swap)

  def insert[A](a: => A): CorngOp[A] =
    apply((a, _))

  def read[A](f: Int => A): CorngOp[A] =
    apply(n => (f(n), n))

  def get: CorngOp[Int] =
    apply(n => (n, n))

  def put(n: Int): CorngOp[Unit] =
    apply(_ => ((), n))

  def distribute[A, B](a: CorngOp[A => B]): A => CorngOp[B] =
    w => a map (_(w))

  implicit val CorngOpMonad: Monad[CorngOp] =
    new Monad[CorngOp] {
      def bind[A, B](a: CorngOp[A])(f: A => CorngOp[B]) =
        a flatMap f
      def point[A](a: => A) =
        insert(a)
    }

  implicit val CorngOpZap: Zap[CorngOp, RngOp] =
    new Zap[CorngOp, RngOp] {
      def zapWith[A, B, C](fa: CorngOp[A], gb: RngOp[B])(f: (A, B) => C) =
        fa.zapWith(gb)(f.curried)
    }
}
               */