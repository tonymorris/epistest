package org.epistest

import scalaz._, Free._

sealed trait RngResume[+A] {
  def map[B](f: A => B): RngResume[B] =
    this match {
      case RngCont(x) =>
        RngCont(x map (_ map f))
      case RngTerm(x) =>
        RngTerm(f(x))
    }
}
case class RngCont[+A](x: RngOp[Rng[A]]) extends RngResume[A]
case class RngTerm[+A](x: A) extends RngResume[A]

