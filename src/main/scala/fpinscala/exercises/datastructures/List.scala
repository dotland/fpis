package fpinscala.exercises.datastructures

import scala.annotation.{newMain, tailrec}
import scala.collection.mutable.ListBuffer

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @main
  def experiment(): Unit =
    // EXERCISE 3.8
    val ls = List(1, 2, 3)
    val res1 = foldRight(ls, Nil: List[Int], Cons(_, _))
    assert(res1 == ls)

    val res2 = foldRightViaLeft(ls, Nil: List[Int], Cons(_, _))
    assert(res2 == ls)

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  // EXERCISE 3.13 HARD
  def foldRightViaLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (a, b) => f(b, a))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => throw new IllegalStateException("empty list")
    case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => throw new IllegalStateException("empty list")
    case Cons(_, t) => Cons(h, t)

  def drop[A](l: List[A], n: Int): List[A] =
    @tailrec
    def go(cur: List[A], n: Int): List[A] =
      if n <= 0 then cur
      else cur match
        case Nil => Nil
        case Cons(_, t) => go(t, n - 1)
        
    go(l, n)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    @tailrec
    def go(cur: List[A]): List[A] = cur match
      case Nil => Nil
      case Cons(h, t) => if f(h) then go(t) else cur

    go(l)

  def init[A](l: List[A]): List[A] =
    @tailrec
    def loop(cur: List[A], rev: List[A]): List[A] = cur match
      case Nil => rev
      case Cons(h, t) => loop(t, Cons(h, rev))
      
    @tailrec
    def go(cur: List[A], rev: List[A]): List[A] = cur match
      case Nil => throw new IllegalStateException("empty list")
      case Cons(_, Nil) => loop(rev, Nil)
      case Cons(h, t) => go(t, Cons(h, rev))
      
    go(l, Nil)

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, r) => 1 + r)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (r, _) => r + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (r, a) => Cons(a, r))

  // Note: stack safe to use foldRightViaLeft
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A], append(_, _))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, List[Int](), (a, b) => Cons(a + 1, b))
  
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String](), (a, b) => Cons(a.toString, b))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRightViaLeft(l, List[B](), (a, bs) => Cons(f(a), bs))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    val buf = new ListBuffer[A]
    @tailrec
    def go(as: List[A]): Unit = as match
      case Nil => ()
      case Cons(h, t) => 
        if f(h) then buf += h
        go(t)
        
    go(as)
    List(buf.toSeq*)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRightViaLeft(as, List[B](), (a, bs) => append(f(a), bs))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else List[A]())

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
    reverse(foldLeft(as, (List[Int](), bs), {
      case ((r, bt), a) => bt match
        case Cons(bh, btt) => (Cons(a + bh, r), btt)
        case _ => (r, List[Int]())
    })._1)

  def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] =
    reverse(foldLeft(as, (List[C](), bs), {
      case ((r, bt), a) => bt match
        case Cons(bh, btt) => (Cons(f(a, bh), r), btt)
        case _ => (r, List[B]())
    })._1)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @tailrec
    def isPrefix(sup: List[A], sub: List[A]): Boolean = sup match
      case Nil => sub == Nil
      case Cons(h1, t1) => sub match
        case Nil => true
        case Cons(h2, t2) => 
          if h1 != h2 then false else isPrefix(t1, t2)

    val subLength = lengthViaFoldLeft(sub)
    
    @tailrec
    def go(sup: List[A], sub: List[A]): Boolean =
      if lengthViaFoldLeft(sup) < subLength then false
      else if isPrefix(sup, sub) then true
      else sup match
        case Nil => sub == Nil
        case Cons(_, t) => go(t, sub)
        
    go(sup, sub)