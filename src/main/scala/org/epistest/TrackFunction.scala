package org.epistest

case class TrackFunction[A, B](f: A => B, s: List[(A, B)]) {
  def apply(a: A): (B, List[(A, B)]) = {
    val b = f(a)
    (b, (a, b) :: s)
  }
}
