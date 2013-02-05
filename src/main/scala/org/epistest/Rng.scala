package org.epistest

import scalaz._, Free._, Scalaz._

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

  def run: A = {
    @annotation.tailrec
    def loop(g: Rng[A], r: java.util.Random): A =
      g.resume match {
        case RngCont(NextDouble(q)) =>
          loop(q(r.nextDouble), r)
        case RngCont(NextLong(q)) =>
          loop(q(r.nextLong), r)
        case RngCont(NextInt(q)) =>
          loop(q(r.nextInt), r)
        case RngTerm(a) =>
          a
      }

    loop(this, new java.util.Random)
  }


}

object Rng {
  private[epistest] def apply[A](f: Free[RngOp, A]): Rng[A] =
    new Rng[A] {
      val free = f
    }

  def int: Rng[Int] =
    NextInt(x => x).lift

  def char: Rng[Char] =
    int map (_.toChar)

  def sequence[T[_], A](x: T[Rng[A]])(implicit T: Traverse[T]): Rng[T[A]] =
    T.sequence(x)


  implicit val RngMonad: Monad[Rng] =
    new Monad[Rng] {
      def bind[A, B](a: Rng[A])(f: A => Rng[B]) =
        a flatMap f
      def point[A](a: => A) =
        Rng(Return(a))
    }

}

object T {
  import Rng._

  def main(args: Array[String]) {
    val r = int flatMap (n => sequence[List, Char](List.fill(n)(char)))
    println(r.run)
  }
}