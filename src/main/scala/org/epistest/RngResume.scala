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

  def free: Rng[A] =
    Rng(this match {
      case RngCont(x) =>
        Suspend(x map (_.free))
      case RngTerm(x) =>
        Return(x)
    })

  def term: Option[A] =
    this match {
      case RngCont(_) =>
        None
      case RngTerm(x) =>
        Some(x)
    }

  def cont: Option[RngOp[Rng[A]]] =
    this match {
      case RngCont(x) =>
        Some(x)
      case RngTerm(x) =>
        None
    }
}
case class RngCont[+A](x: RngOp[Rng[A]]) extends RngResume[A]
case class RngTerm[+A](x: A) extends RngResume[A]

