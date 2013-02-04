package org.epistest

import scalaz._

sealed trait LabelledGen[+L, -A, +B] {
  val value: A => Rng[Option[L] \/ B]
}
