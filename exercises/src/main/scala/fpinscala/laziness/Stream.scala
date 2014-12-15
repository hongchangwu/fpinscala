package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B =
    this match {
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z
    }

  def reverse: Stream[A] =
    foldLeft[Stream[A]](empty)((z, a) => cons(a, z))

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight[List[A]](Nil)(_ :: _)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) =>  cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, z) => p(a) && z)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, z) => if (p(a)) cons(a, z) else empty)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a, z) => cons(f(a), z))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, z) => if (p(a)) cons(a, z) else z)

  def append[B >: A](b: Stream[B]): Stream[B] =
    foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((a, z) => f(a) append z)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zip[B](b: Stream[B]): Stream[(A, B)] =
    zipWith(b)((_, _))

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(b)((_, _))

  def zipWithAll[B, C](b: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), empty[B]))
      case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (empty[A], t2()))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zip(s) forAll (x => x._1 == x._2)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case x @ Cons(h, t) => Some(x, t())
      case _ => None
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    cons(z, unfold((this.reverse, z)) {
      case (Cons(h, t), z) =>
        val z1 = f(h(), z)
        Some(z1, (t(), z1))
      case _ => None
    }).reverse

  def scanRightViaFoldRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight((Stream(z), z)) {
      case (a, (s, z)) =>
        val z1 = f(a, z)
        (cons(z1, s), z1)
    }._1
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def recur(a: Int, b: Int): Stream[Int] = {
      cons(a, recur(b, a + b))
    }
    recur(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, z1)) => cons(a, unfold(z1)(f))
      case None => empty
    }
  }

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

}