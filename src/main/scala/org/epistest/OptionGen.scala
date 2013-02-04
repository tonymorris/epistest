package org.epistest

sealed trait OptionGen[-A, +B] {
  val value: A => Rng[Option[B]]
}
