package org.epistest

import scalaz._

sealed trait Decision[-A] {
  val decide: A => Boolean

  def apply(a: A): Boolean =
    decide(a)

  def contramap[B](f: B => A): Decision[B] =
    Decision(decide compose f)

  def \/[B](d: => Decision[B]): Decision[(A, B)] =
    Decision {
      case (a, b) => decide(a) || d.decide(b)
    }

  def /\[B](d: => Decision[B]): Decision[(A, B)] =
    Decision {
      case (a, b) => decide(a) && d.decide(b)
    }

  def ==>[B](d: => Decision[B]): Decision[(A, B)] =
    Decision {
      case (a, b) => !decide(a) || d.decide(b)
    }

  def <=>[B](d: Decision[B]): Decision[(A, B)] =
    Decision {
      case (a, b) => decide(a) == d.decide(b)
    }

  def /=[B](d: Decision[B]): Decision[(A, B)] =
    Decision {
      case (a, b) => decide(a) != d.decide(b)
    }

  def unary_! : Decision[A] =
    Decision(!apply(_))

  def either[B](d: Decision[B]): Decision[A \/ B] =
    Decision {
      case -\/(a) => decide(a)
      case \/-(b) => d.decide(b)
    }

  def all[F[_], AA <: A](implicit F: Foldable[F]): Decision[F[AA]] =
    Decision(F.all(_)(decide))

  def any[F[_], AA <: A](implicit F: Foldable[F]): Decision[F[AA]] =
    Decision(F.any(_)(decide))

}

object Decision {
  def apply[A](f: A => Boolean): Decision[A] =
    new Decision[A] {
      val decide = f
    }

  def True[A]: Decision[A] =
    Decision(_ => true)

  def False[A]: Decision[A] =
    Decision(_ => false)

  implicit val BooleanDecision: Decision[Boolean] =
    apply(identity)

  implicit val UnitDecision: Decision[Unit] =
    True

  implicit val IntDecision: Decision[Int] =
    apply(_ != 0)

  implicit val LongDecision: Decision[Long] =
    apply(_ != 0L)

  implicit val ShortDecision: Decision[Short] =
    apply(_ != 0)

  implicit val ByteDecision: Decision[Byte] =
    apply(_ != 0)

  implicit val CharDecision: Decision[Char] =
    apply(_ != 0)

  implicit val StringDecision: Decision[String] =
    apply(!_.isEmpty)

  implicit def OptionDecision[A]: Decision[Option[A]] =
    apply(_.isDefined)

  implicit def ListDecision[A]: Decision[List[A]] =
    apply(!_.isEmpty)

  implicit def VectorDecision[A]: Decision[Vector[A]] =
    apply(!_.isEmpty)

  implicit def StreamDecision[A]: Decision[EphemeralStream[A]] =
    apply(!_.isEmpty)

}
