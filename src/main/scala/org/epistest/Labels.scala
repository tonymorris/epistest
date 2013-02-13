package org.epistest

import scalaz._

sealed trait Labels {
  import Labels._

  val value: MaybeEmptyList[Label]

  def list: List[Label] =
    value.list

  def either: List[Label] \/ NonEmptyList[Label] =
    value.either

  def head: Option[Label] =
    value.head

  def ::(l: Label): Labels =
    Labels(l :: value)

  def ++(l: Labels): Labels =
    Labels(value ++ l.value)

  def foldRight[B](z: B)(f: (Label, B) => B): B =
    value.foldRight(z)(f)

  def foldLeft[B](z: B)(f: (B, Label) => B): B =
    value.foldLeft(z)(f)

  def eachlabel(f: Label => Label): Labels =
    Labels(value map f)

  def eachlabels(f: Label => Labels): Labels =
    Labels(value flatMap (f(_).value))
}

object Labels {
  type Label = String

  def apply(x: MaybeEmptyList[Label]): Labels =
    new Labels {
      val value = x
    }

  def single(l: Label): Labels =
    apply(MaybeEmptyList.single(l))
}
