package org.epistest

sealed trait GenResult[+A] {
  val size: Size
  val rng: Rng[A]

  def map[B](f: A => B): GenResult[B] =
    GenResult(size, rng map f)

}

object GenResult {
  def apply[A](s: Size, r: Rng[A]): GenResult[A] =
    new GenResult[A] {
      val size = s
      val rng = r
    }
}
