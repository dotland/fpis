package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def isLeft: Boolean = this match
    case Left(_) => true
    case Right(_) => false

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case _ => this.asInstanceOf[Either[E, B]]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case _ => this.asInstanceOf[Either[EE, B]]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_) => b
    case _ => this

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => eb.map(b => f(a, b)))

object Either:
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil)): (a, acc) =>
      f(a).map2(acc)(_ :: _)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.find(_.isLeft) match
      case None => Right(es.collect { case Right(a) => a} )
      case Some(e) => e.asInstanceOf[Either[E, List[A]]]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es), Right(_)) => Left(es)
      case (Right(_), Left(es)) => Left(es)
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldRight(Right(Nil): Either[List[E], List[B]]): (a, acc) =>
      map2All(f(a), acc, _ :: _)

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(as, identity)
