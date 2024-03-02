package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop: 
  /* Solution 8.3
  outer =>
  def check: Boolean
  def &&(that: Prop): Prop = new Prop:
    override def check: Boolean = outer.check && that.check
  */
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

object Prop:
  opaque type FailedCase = String
  opaque type SuccessCount = Int
  
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

  
opaque type Gen[+A] = State[RNG, A]
  
object Gen:
  def unit[A](a: => A): Gen[A] = State.unit(a)

  def boolean: Gen[Boolean] = State { rng => 
    val (i, rng2) = choose(0, 2).next(rng)
    (i == 0, rng2)
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
//    State(RNG.nonNegativeLessThan(stopExclusive - start)).map(i => start + i) // SO Exception
    State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start))

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = State { rng =>
      val (a, rng2) = self.next(rng)
      f(a).next(rng2)
    }

    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

    def listOfN(sizeGen: Gen[Int]): Gen[List[A]] =
      sizeGen.flatMap(listOfN)

/*
trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
*/
