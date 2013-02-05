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
    val r = new java.util.Random

    @annotation.tailrec
    def loop(g: Rng[A]): A =
      g.resume match {
        case RngCont(NextDouble(q)) =>
          loop(q(r.nextDouble))
        case RngCont(NextLong(q)) =>
          loop(q(r.nextLong))
        case RngCont(NextInt(q)) =>
          loop(q(r.nextInt))
        case RngTerm(a) =>
          a
      }

    loop(this)
  }


}

object Rng {
  def apply[A](f: Free[RngOp, A]): Rng[A] =
    new Rng[A] {
      val free = f
    }

  def int: Rng[Int] =
    NextInt(x => x).lift

  def sequenceL[A](x: List[Rng[A]]): Rng[List[A]] = {
    x.foldRight[Rng[List[A]]](Rng(Return(Nil))) {
      case (a, b) => for {
        aa <- a
        bb <- b
      } yield aa::bb
    }
  }

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

object Main {
  import Rng._

  def main(args: Array[String]) {
    def g(n: Int): List[Rng[Int]] = List.fill(n)(int)
    def h(n: Int): Rng[List[Int]] = sequenceL[Int](g(n))
    val r = int flatMap (n => h(math.abs(n % 10)))
    println(r.run)
  }
}