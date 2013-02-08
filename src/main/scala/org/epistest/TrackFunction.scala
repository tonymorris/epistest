package org.epistest

sealed trait TrackResult[A, B] {
  val parameter: A
  val result: B
}

object TrackResult {
  def apply[A, B](p: A, r: B): TrackResult[A, B] =
    new TrackResult[A, B] {
      val parameter = p
      val result = r
    }
}

sealed trait TrackResults[A, B] {
  val results: List[TrackResult[A, B]]

  def ::(r: TrackResult[A, B]): TrackResults[A, B] =
    TrackResults(r :: results)

  def ++(r: TrackResults[A, B]): TrackResults[A, B] =
    TrackResults(results ::: r.results)
}

object TrackResults {
  def apply[A, B](r: List[TrackResult[A, B]]): TrackResults[A, B] =
    new TrackResults[A, B] {
      val results = r
    }

  def empty[A, B]: TrackResults[A, B] =
    apply(Nil)
}

sealed trait TrackRun[A, B, X] {
  val result: X
  val tracks: TrackResults[A, B]

  def map[Y](f: X => Y): TrackRun[A, B, Y] =
    TrackRun(f(result), tracks)
}

object TrackRun {
  def apply[A, B, X](r: X, t: TrackResults[A, B]): TrackRun[A, B, X] =
    new TrackRun[A, B, X] {
      val result = r
      val tracks = t
    }
}

sealed trait Track[A, B, X] {
  def run(f: A => B, rs: TrackResults[A, B]): TrackRun[A, B, X]

  def apply(f: A => B): TrackRun[A, B, X] =
    run(f, TrackResults.empty)

  def map[Y](f: X => Y): Track[A, B, Y] =
    Track((k, r) => run(k, r) map f)

  def flatMap[Y](f: X => Track[A, B, Y]): Track[A, B, Y] =
    Track((k, r) => {
      val g = run(k, r)
      f(g.result) run (k, g.tracks)
    })

}

object Track {
  private def apply[A, B, X](q: (A => B, TrackResults[A, B]) => TrackRun[A, B, X]): Track[A, B, X] =
    new Track[A, B, X] {
      def run(f: A => B, rs: TrackResults[A, B]) =
        q(f, rs)
    }

  type =+>[A, B] = Track[A, B, B]

  sealed trait TrackApply[B] {
    def apply[A](a: A): A =+> B =
      new Track[A, B, B] {
        def run(q: A => B, r: TrackResults[A, B]) = {
          val b = q(a)
          TrackRun(b, TrackResult(a, b) :: r)
        }
      }
  }

  def track[B]: TrackApply[B] =
    new TrackApply[B] {}

}
