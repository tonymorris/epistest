package org.epistest

case class TrackFunction[A, B](f: A => B, s: List[(A, B)]) {
  def apply(a: A): Tracked[A, B] = {
    val b = f(a)
    Tracked(b, TrackFunction[A, B](f, (a, b) :: s))
  }
}

case class Tracked[A, B](result: B, t: TrackFunction[A, B]) {
  def apply(a: A): Tracked[A, B] =
    t(a)
}

case class Tracking[A, B](run: TrackFunction[A, B] => Tracked[A, B]) {

}

object Tracking {
  def tracking[A, B](a: A): Tracking[A, B] =
    Tracking(f => f(a))
}

case class Track[A, B](run: List[(A, B)] => (A => B, List[(A, B)])) {
  def apply(a: A): Track[A, B] =
    Track(s => {
      val (f, t) = run(s)
      val b = f(a)
      (f, (a, b) :: t)
    })

  def bind(f: B => Track[A, B]): Track[A, B] =
    Track(s => {
      val (f, t) = run(s)
      (f, t match {
        case Nil => Nil
        case (a, _)::y => {
          val b = f(a)
          (a, b) :: t
        }
      })
    })

}
    /*
case class TrackFunction2[A, B](f: A => B) {
  def apply(a: A): Track[A, B] =
    Track(s => {
      val b = f(a)
      (b, f, (a, b) :: s)
    })
}
    */