package org.epistest

import scalaz._, Scalaz._, NonEmptyList._

sealed trait Interval[+A] {
  val min: A
  val max: A

  def map[B: Order](f: A => B): Interval[B] =
    Interval(f(min), f(max))

  def merge[AA >: A](i: Interval[AA])(implicit E: Enum[AA]): Option[Interval[AA]] =
    if(((min lte i.max) && ((max: AA).succ gte i.min)) || ((i.min lte max) && (i.max gte min)))
      Some(Interval(min min i.min, max max i.max))
    else
      None

  def isPoint[AA >: A](implicit E: Equal[AA]): Boolean =
    (min: AA) === max

  def pair: (A, A) =
    (min, max)

  def zipWith[AA >:A, B, C](i: Interval[B])(f: A => B => C)(implicit O: Order[C]): Interval[C] =
    Interval(f(min)(i.min), f(max)(i.max))

  def zip[AA >:A, B](i: Interval[B]): Interval[(AA, B)] =
    Interval.interval((min, i.min), (max, i.max))

  def ***[AA >:A, B](i: Interval[B]): Interval[(AA, B)] =
    zip(i)

  def list: List[A] =
    List(min, max)

  def list1: NonEmptyList[A] =
    nel(min, List(max))

  def ===[AA >: A](i: Interval[AA])(implicit E: Equal[AA]): Boolean =
    pair === i.pair

  def compare[AA >: A](i: Interval[AA])(implicit O: Order[AA]): Ordering =
    pair ?|? i.pair

  def show[AA >: A](implicit E: Equal[AA], S: Show[AA]): Cord =
    '[' -: ((min: AA).show ++ (if(isPoint[AA]) Cord.empty else '|' -: (max: AA).show)) :- ']'

  def |+|[AA >: A](i: Interval[AA])(implicit O: Order[AA], S: Semigroup[AA]): Interval[AA] =
    zipWith(i)(a => (a: AA) |+| _)

}

object Interval {
  def apply[A](i: A, x: A)(implicit O: Order[A]): Interval[A] = {
    if(i lte x)
      interval(i, x)
    else
      interval(x, i)
  }

  // i >= x must hold
  private[epistest] def interval[A](i: A, x: A): Interval[A] =
    new Interval[A] {
      val min = i
      val max = x
    }

  def point[A](a: A): Interval[A] =
    new Interval[A] {
      val min = a
      val max = a
    }

  def unzip[A, B](i: Interval[(A, B)]): (Interval[A], Interval[B]) =
    (interval(i.min._1, i.max._1), interval(i.min._2, i.max._2))

  def counzip[A, B](i: Interval[A] \/ Interval[B]): Interval[A \/ B] =
    i match {
      case -\/(a) =>
        new Interval[A \/ B] {
          val min = a.min.left
          val max = a.max.left
        }
      case \/-(b) =>
        new Interval[A \/ B] {
          val min = b.min.right
          val max = b.max.right
        }
    }

  implicit def IntervalEqual[A](implicit E: Equal[A]): Equal[Interval[A]] =
    Equal.equal(_ === _)

  implicit def IntervalOrder[A](implicit O: Order[A]): Order[Interval[A]] =
    Order.order(_ compare _)

  implicit def IntervalShow[A](implicit E: Equal[A], S: Show[A]): Show[Interval[A]] =
    Show.show(_.show)

  implicit val IntervalZip: Zip[Interval] =
    new Zip[Interval] {
      def zip[A, B](a: => Interval[A], b: => Interval[B]) =
        a zip b
    }

  object Range {
    val unit: Interval[Unit] =
      point(())

    val boolean: Interval[Boolean] =
      interval(false, true)

    val byte: Interval[Byte] =
      interval(Byte.MinValue, Byte.MaxValue)

    val char: Interval[Char] =
      interval(Char.MinValue, Char.MaxValue)

    val double: Interval[Double] =
      interval(Double.MinValue, Double.MaxValue)

    val float: Interval[Float] =
      interval(Float.MinValue, Float.MaxValue)

    val int: Interval[Int] =
      interval(Int.MinValue, Int.MaxValue)

    val long: Interval[Long] =
      interval(Long.MinValue, Long.MaxValue)

    val short: Interval[Short] =
      interval(Short.MinValue, Short.MaxValue)

    val digit: Interval[Digit] =
      interval(Digit._0, Digit._9)

    val ordering: Interval[Ordering] =
      interval(Ordering.LT, Ordering.GT)


  }

}
