package org.epistest

sealed trait GenResult[+A] {
  val size: Size
  val value: A

  def map[B](f: A => B): GenResult[B] =
    GenResult(size, f(value))

}

object GenResult {
  def apply[A](s: Size, v: A): GenResult[A] =
    new GenResult[A] {
      val size = s
      val value = v
    }
}
