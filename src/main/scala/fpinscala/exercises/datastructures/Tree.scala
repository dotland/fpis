package fpinscala.exercises.datastructures

import fpinscala.exercises.datastructures.Tree.Branch

import scala.List

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
      
  def toList: List[A] = this match
    case Leaf(v) => List(v)
    case Branch(l, r) => l.toList ++ r.toList
  
  def sizeViaFold: Int = fold(_ => 1, 1 + _ + _)
  
  def depthViaFold: Int = fold(_ => 0, 1 + _.max(_))
  
  def mapViaFold[B](f: A => B): Tree[B] = 
    fold(a => Leaf(f(a)), Branch.apply)

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  def fill[A](n: Int)(elem: => A): Tree[A] =
    if n < 1 then null
    else if n == 1 then Leaf(elem)
    else
      val h = n / 2
      Branch(fill(n - h)(elem), fill(h)(elem))

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => l.maximum max r.maximum

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(identity, _ max _)
