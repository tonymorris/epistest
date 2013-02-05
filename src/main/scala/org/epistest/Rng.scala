package org.epistest

import scalaz._, Free._, Scalaz._, NonEmptyList._

sealed trait Rng[+A] {
  val free: Free[RngOp, A]

  import Rng._

  def map[B](f: A => B): Rng[B] =
    Rng(free map f)

  def flatMap[B](f: A => Rng[B]): Rng[B] =
    Rng(free flatMap (f(_).free))

  def ap[X](f: Rng[A => X]): Rng[X] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[X](q: Rng[X]): Rng[(A, X)] =
    for {
      a <- this
      x <- q
    } yield (a, x)

  def zap[G[+_], B](fs: Cofree[G, A => B])(implicit G: Functor[G], d: Zap[RngOp, G]): B =
    free.zap(fs)

  def foldRun[B, AA >: A](b: B)(f: (B, RngOp[Rng[AA]]) => (B, Rng[AA])): (B, AA) =
    free.foldRun[B, AA](b)((bb, t) => f(bb, t map (Rng(_))) :-> (_.free))

  def resume: RngResume[A] =
    free.resume match {
      case -\/(x) => RngCont(x map (Rng(_)))
      case \/-(x) => RngTerm(x)
    }

  def run: A = {
    @annotation.tailrec
    def loop(g: Rng[A], r: java.util.Random): A =
      g.resume match {
        case RngCont(NextDouble(q)) =>
          loop(q(r.nextDouble), r)
        case RngCont(NextLong(q)) =>
          loop(q(r.nextLong), r)
        case RngCont(NextInt(q)) =>
          loop(q(r.nextInt), r)
        case RngTerm(a) =>
          a
      }

    loop(this, new java.util.Random)
  }

  def maph[G[+_]](f: RngOp ~> G)(implicit G: Functor[G]): Free[G, A] =
    free mapSuspension f

  def mapr(f: RngOp ~> RngOp): Rng[A] =
    Rng(free mapFirstSuspension f)

  def go[AA >: A](f: RngOp[Rng[AA]] => Rng[AA]): AA =
    free.go[AA](r => f(r map (Rng(_))).free)

  def gen[X]: Gen[X, A] =
    Gen(_ => this)

  def |+|[AA >: A](x: Rng[AA])(implicit S: Semigroup[AA]): Rng[AA] =
    for {
      a <- this
      b <- x
    } yield S.append(a, b)

  def many: Rng[List[A]] =
    nextInt flatMap (n => sequence(List.fill(n)(this)))

  def many1: Rng[NonEmptyList[A]] =
    nextInt flatMap (n => sequence(nel(this, List.fill(n)(this))))

  def option: Rng[Option[A]] =
    chooseBoolean flatMap (p => sequence[Option, A](if(p) None else Some(this)))
}

object Rng {
  private[epistest] def apply[A](f: Free[RngOp, A]): Rng[A] =
    new Rng[A] {
      val free = f
    }

  def nextDouble: Rng[Double] =
    NextDouble(x => x).lift

  def nextLong: Rng[Long] =
    NextLong(x => x).lift

  def nextInt: Rng[Int] =
    NextInt(x => x).lift

  def insert[A](a: A): Rng[A] =
    Rng(Return(a))

  def chooseLong(l: Long, h: Long): Rng[Long] =
    nextLong map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      ll + math.abs(x % (hh - ll + 1))
    })

  def chooseDouble(l: Double, h: Double): Rng[Double] = {
    nextDouble map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      val diff = hh - ll
      if(diff == 0)
        ll
      else
        ll + math.abs(x * diff + ll)
    })
  }

  def chooseInt(l: Int, h: Int): Rng[Int] =
    nextInt map (x => {
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      ll + math.abs(x % (hh - ll + 1))
    })

  def chooseBoolean: Rng[Boolean] =
    chooseInt(0, 1) map (_ == 0)

  def oneofL[A](x: NonEmptyList[A]): Rng[A] =
    chooseInt(0, x.length - 1) map (x toList _)

  def oneof[A](a: A, as: A*): Rng[A] =
    oneofL(NonEmptyList(a, as: _*))

  def sequence[T[_], A](x: T[Rng[A]])(implicit T: Traverse[T]): Rng[T[A]] =
    T.sequence(x)

  def sequencePair[X, A](x: X, r: Rng[A]): Rng[(X, A)] =
    sequence[({type f[x] = (X, x)})#f, A]((x, r))

  def frequencyL[A](x: NonEmptyList[(Int, Rng[A])]): Rng[A] = {
    val t = x.foldLeft(0) {
      case (a, (b, _)) => a + b
    }

    @annotation.tailrec
    def pick(n: Int, l: NonEmptyList[(Int, Rng[A])]): Rng[A] = {
      val (q, r) = l.head
      if(n <= q)
        r
      else l.tail match {
        case Nil => r
        case e::es => pick(n - q, nel(e, es))
      }
    }

    for {
      n <- chooseInt(1, t)
      w <- pick(n, x)
    } yield w
  }

  implicit val RngMonad: Monad[Rng] =
    new Monad[Rng] {
      def bind[A, B](a: Rng[A])(f: A => Rng[B]) =
        a flatMap f
      def point[A](a: => A) =
        insert(a)
    }

  implicit def RngMonoid[A](implicit M: Monoid[A]): Monoid[Rng[A]] =
    new Monoid[Rng[A]] {
      def append(r1: Rng[A], r2: => Rng[A]) =
        r1 |+| r2

      def zero =
        insert(M.zero)
    }
}

