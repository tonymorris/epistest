package org.epistest

import scalaz._

sealed trait Perturb[A, -B]{
  val perturb: A => B => A

  def apply(a: A, b: B): A =
    perturb(a)(b)

  def contramap[C](f: C => B): Perturb[A, C] =
    Perturb(a => c => perturb(a)(f(c)))

  def xmap[X](f: A => X, g: X => A): Perturb[X, B] =
    Perturb(x => b => f(perturb(g(x))(b)))

  def ***[C, D](p: Perturb[C, D]): Perturb[(A, C), (B, D)] =
    Perturb {
      case (a, c) => {
        case (b, d) => (perturb(a)(b), p.perturb(c)(d))
      }
    }

  def &&&[C](p: Perturb[A, C])(implicit S: Semigroup[A]): Perturb[A, (B, C)] =
    Perturb(a => {
      case (b, c) => S.append(perturb(a)(b), p.perturb(a)(c))
    })

  def cozipA[BB <: B, X](p: Perturb[X, BB]): Perturb[A \/ X, BB] =
    Perturb {
      case -\/(a) => b => -\/(perturb(a)(b))
      case \/-(x) => b => \/-(p.perturb(x)(b))
    }

  def cozipB[C](p: Perturb[A, C]): Perturb[A, B \/ C] =
    Perturb(a => {
      case -\/(b) => perturb(a)(b)
      case \/-(c) => p.perturb(a)(c)
    })

  def zipA[BB <: B, C](p: Perturb[C, BB]): Perturb[(A, C), BB] =
    Perturb {
      case (a, c) => b => (perturb(a)(b), p.perturb(c)(b))
    }

  def zipB[C](p: Perturb[A, C])(implicit S: Semigroup[A]): Perturb[A, (B, C)] =
    Perturb(a => {
      case (b, c) => S.append(perturb(a)(b), p.perturb(a)(c))
    })

  def uncurry: (A, B) => A =
    Function.uncurried(perturb)

  def tuple: ((A, B)) => A =
    uncurry.tupled

  def flip: B => A => A =
    b => a => perturb(a)(b)

  def flipU: (B, A) => A =
    Function.uncurried(flip)

  def flipT: ((B, A)) => A =
    flipU.tupled

  def endo(b: B): Endo[A] =
    Endo(flip(b))
}

object Perturb {
  def apply[A, B](f: A => B => A): Perturb[A, B] =
    new Perturb[A, B] {
      val perturb = f
    }

  def uncurry[A, B](f: (A, B) => A): Perturb[A, B] =
    apply(f.curried)

  def tuple[A, B](f: ((A, B)) => A): Perturb[A, B] =
    apply(a => b => f((a, b)))

  def flip[A, B](f: B => A => A): Perturb[A, B] =
    apply(a => b => f(b)(a))

  def flipU[A, B](f: (B, A) => A): Perturb[A, B] =
    flip(f.curried)

  def flipT[A, B](f: ((B, A)) => A): Perturb[A, B] =
    apply(a => b => f((b, a)))

  def endo[A, B](f: B => Endo[A]): Perturb[A, B] =
    flip(f(_).run)

  def variant[A, B](f: B => A): Perturb[A, B] =
    apply(_ => f)

  trait PerturbTarget[B] {
    def apply[A](f: A => A): Perturb[A, B] =
      Perturb(a => _ => f(a))
  }
  def target[B]: PerturbTarget[B] =
    new PerturbTarget[B] {}

  trait PerturbTargetE[B] {
    def apply[A](f: Endo[A]): Perturb[A, B] =
      Perturb(a => _ => f(a))
  }
  def targetE[B]: PerturbTarget[B] =
    new PerturbTarget[B] {}

  trait PerturbInsert[B] {
    def apply[A](a: => A): Perturb[A, B] =
      Perturb(_ => _ => a)
  }
  def insert[A, B](a: => A): Perturb[A, B] =
    apply(_ => _ => a)

  def still[A, B]: Perturb[A, B] =
    target(a => a)

  def first[A]: Perturb[A, A] =
    still

  def second[A]: Perturb[A, A] =
    variant(z => z)

}
