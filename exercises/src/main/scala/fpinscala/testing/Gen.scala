package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop {
    (n, rng) =>
      run(n, rng) match {
        case Passed | Proved => p.run(n, rng)
        case x => x
      }
  }

  def ||(p: Prop) = Prop {
    (n, rng) =>
      run(n, rng) match {
        case res if res.isFalsified => p.run(n, rng)
        case x => x
      }
  }

}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(e.toString, i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(n => (n & 1) == 0))
  }

  def double: Gen[Double] = {
    Gen(State(RNG.double))
  }

  def pairs(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val l = choose(start, stopExclusive)
    val r = choose(start, stopExclusive)
    l.flatMap(a => r.map(b => (a, b)))
  }

  def string(n: Int): Gen[String] = {
    val g = choose(33, 123)
    val builder = new StringBuilder()
    listOfN(n, g).map { ln =>
      ln.foreach(c => builder.append(c.toChar))
      builder.toString
    }
  }

  //Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    double.flatMap(d => if (d * (g1._2 + g2._2) < g1._2) g1._1 else g2._1)
  }

}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  def unsized: SGen[A] = {
    SGen(_ => this)
  }

}

case class SGen[A](forSize: Int => Gen[A]) {

  //Exercise 8.11
  def map[B](f: A => B): SGen[B] = {
    SGen(forSize(_: Int).map(f))
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen {
      i: Int =>
        val tmp: Gen[A] = forSize(i)
        val g: (A => Gen[B]) = f(_).forSize(i)
        tmp.flatMap(g)
    }
  }

}

