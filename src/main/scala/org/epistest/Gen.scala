package org.epistest

sealed trait Gen[~>[-_, +_], -A, +B] {
  val value: A ~> Rng[B]

}
