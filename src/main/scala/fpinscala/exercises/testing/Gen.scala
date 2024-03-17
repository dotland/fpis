package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import Prop.Result.{Falsified, Passed, Proved}
import fpinscala.answers.testing.{Gen, Prop}
import fpinscala.answers.testing.Prop.forAll
import fpinscala.exercises.datastructures.Tree

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.targetName

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// trait Prop:
  /* Solution 8.3
  outer =>
  def check: Boolean
  def &&(that: Prop): Prop = new Prop:
    override def check: Boolean = outer.check && that.check
  */
  // def check: Either[(FailedCase, SuccessCount), SuccessCount]

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true
      case Proved => false

  /* Produce an infinite random lazy list from a `Gen` and a starting `RNG`. */
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop:
    (n, rng) =>
      randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map:
        case (a, i) =>
          try
            if f(a) then Passed else Falsified(a.toString, i)
          catch
            case e: Exception => Falsified(buildMsg(a, e), i)
      .find(_.isFalsified).getOrElse(Passed)

  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] =
        LazyList.from(0).take((n.toInt min max.toInt) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map[Prop](p => (max, _, rng) => p(max, casesPerSize, rng)).toList.reduce(_ && _)
      prop(max, n, rng)

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  extension (self: Prop)

    def check(
               maxSize: MaxSize = 100,
               testCases: TestCases = 100,
               rng: RNG = RNG.Simple(System.currentTimeMillis)
             ): Result =
      self(maxSize, testCases, rng)

    def run(maxSize: MaxSize = 100,
            testCases: TestCases = 100,
            rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")

    def tag(msg: String): Prop = (max, n, rng) =>
      self(max, n, rng) match
        case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
        case x => x

    @targetName("and")
    def &&(that: Prop): Prop = (max, n, rng) =>
      self.tag("left")(max, n, rng) match
        case r if r.isFalsified => r
        case _ => that.tag("right")(max, n, rng)

    @targetName("or")
    def ||(that: Prop): Prop = (max, n, rng) =>
      self.tag("left")(max, n, rng) match
        case Falsified(msg, _) => that.tag("right").tag(msg)(max, n, rng)
        case x => x


opaque type Gen[+A] = State[RNG, A]
opaque type SGen[+A] = Int => Gen[A]

object Gen:
  def unit[A](a: => A): Gen[A] = State.unit(a)

  def boolean: Gen[Boolean] = State { rng => 
    val (i, rng2) = choose(0, 2).next(rng)
    (i == 0, rng2)
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
//    State(RNG.nonNegativeLessThan(stopExclusive - start)).map(i => start + i) // SO Exception
    State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    State(RNG.double).flatMap { d =>
      if d < g1._2 then g1._1 else g2._1
    }

  extension [A](self: Gen[A])
    def map[B](f: A => B): Gen[B] =
      State.map(self)(f)

    def map2[B,C](that: Gen[B])(f: (A, B) => C): Gen[C] =
      State.map2(self)(that)(f)
    
    def flatMap[B](f: A => Gen[B]): Gen[B] = State { rng =>
      val (a, rng2) = self.next(rng)
      f(a).next(rng2)
    }

    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))
      
    def treeOfN(n: Int): Gen[Tree[A]] =
      State.sequence(Tree.fill(n)(self))

    def listOfN(sizeGen: Gen[Int]): Gen[List[A]] =
      sizeGen.flatMap(listOfN)
      
    def treeOfN(sizeGen: Gen[Int]): Gen[Tree[A]] =
      sizeGen.flatMap(treeOfN)
      
    def list: SGen[List[A]] = n => self.listOfN(n)
    
    def tree: SGen[Tree[A]] = n => self.treeOfN(n)

    def nonEmptyList: SGen[List[A]] = n => self.listOfN(n + 1)
    
    def nonEmptyTree: SGen[Tree[A]] = n => self.treeOfN(n + 1)

    def unsized: SGen[A] = _ => self

    @targetName("product")
    def **[B](gb: Gen[B]): Gen[(A, B)] =
      map2(gb)((_, _))

  def apply[A](s: State[RNG, A]): Gen[A] = s

  val smallInt: Gen[Int] = Gen.choose(-10, 10)
  val int: Gen[Int] = Gen(State(RNG.int))
  val double: Gen[Double] = Gen(State(RNG.double))


/*
trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???
*/

object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(n: Int): Gen[A] = self(n)

    def map[B](f: A => B): SGen[B] =
      n => self(n).map(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      n => self(n).flatMap(a => f(a)(n))

    def **[B](s2: SGen[B]): SGen[(A, B)] =
      n => Gen.**(apply(n))(s2(n))


opaque type Cogen[-A] = (A, RNG) => RNG

object Cogen:
  def fn[A, B](in: Cogen[A], out: Gen[B]): Gen[A => B] =
    State[RNG, A => B]: rng =>
      val (seed, rng2) = rng.nextInt
      val f = (a: A) => out.run(in(a, rng2))._1
      (f, rng2)

  def cogenInt: Cogen[Int] = (i, rng) =>
    val (seed, rng2) = rng.nextInt
    RNG.Simple(seed.toLong ^ i.toLong)

  // We can now write properties that depend on arbitrary functions
  def takeWhilePropInt =
    Prop.forAll(Gen.int.list ** fn(cogenInt, Gen.boolean).unsized) { (ys, f) =>
      ys.takeWhile(f).forall(f)
    }

  // And we can further generalize those properties to be parameterized by types which are not relevant
  def takeWhileProp[A](ga: Gen[A], ca: Cogen[A]) =
    Prop.forAll(ga.list ** fn(ca, Gen.boolean).unsized) { (ys, f) =>
      val res = ys.takeWhile(f)
      res.forall(f) && res.sizeCompare(ys) <= 0
    }

  def takeWhileDropWhileProp[A](ga: Gen[A], ca: Cogen[A]) =
    Prop.forAll(ga.list ** fn(ca, Gen.boolean).unsized) { (ys, f) =>
      ys == ys.takeWhile(f) ++ ys.dropWhile(f)
    }
