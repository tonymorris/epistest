package org.epistest

import Labels._

sealed trait GenResult[+A] {
  val size: Size
  val labels: Labels
  val value: A

  def map[B](f: A => B): GenResult[B] =
    GenResult(size, labels, f(value))

  def ::(l: Label): GenResult[A] =
    GenResult(size, l :: labels, value)

  def ++(l: Labels): GenResult[A] =
    GenResult(size, labels ++ l, value)

  def :::(l: Labels): GenResult[A] =
      GenResult(size, l ++ labels, value)


}

object GenResult {
  def apply[A](s: Size, l: Labels, v: A): GenResult[A] =
    new GenResult[A] {
      val size = s
      val labels = l
      val value = v
    }
}
