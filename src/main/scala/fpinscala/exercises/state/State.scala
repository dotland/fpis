package fpinscala.exercises.state

import fpinscala.exercises.datastructures.Tree
import fpinscala.exercises.datastructures.Tree.{Branch, Leaf}
import fpinscala.exercises.state.Input.{Coin, Turn}
import fpinscala.exercises.state.RNG.{map2, unit}


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    (if n == Int.MinValue then Int.MaxValue else math.abs(n), rng2)

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (if i == 0 then 0 else (i.toDouble - 1) / Int.MaxValue, r)

  def _double: Rand[Double] =
    map(nonNegativeInt) { i => 
      if i == 0 then 0 else (i.toDouble - 1) / Int.MaxValue
    }

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    (i -> d, r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    (d -> i, r2)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    var lastRng = rng
    val ls = List.unfold((rng, count)):
      case (r, c) if c > 0 => 
        val (i, r2) = r.nextInt
        lastRng = r2
        Some(i, (lastRng, c - 1))
      case _ => None

    (ls, lastRng)

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => 
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A])) { (r, acc) => map2(r, acc)(_ :: _) }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r) { a => unit(f(a)) }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(rb) { b => flatMap(ra) { a => unit(f(a, b)) } }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s =>
        val (a, s1) = run(s)
        (f(a), s1)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s =>
        val (a, s1) = run(s)
        val (b, s2) = sb(s1)
        (f(a, b), s2)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = run(s)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =  s => (a, s)

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](Nil)) { (s, acc) => s.map2(acc)(_ :: _) }

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil)) { (a, acc) => f(a).map2(acc)(_ :: _) }

  def sequence[S, A](ss: Tree[State[S, A]]): State[S, Tree[A]] =
    ss.fold( _.map(Leaf.apply), (s1, s2) => s1.map2(s2)(Branch(_, _)) )

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State { machine =>
      val res = inputs.foldLeft(machine) {
        case (m @ Machine(_, 0, _), _) => m
        case (Machine(true, candies, coins), Coin) =>
          Machine(false, candies, coins + 1)
        case (Machine(false, candies, coins), Turn) =>
          Machine(true, candies - 1, coins)
        case (m, _) => m
      }
      ((res.coins, res.candies), res)
    }