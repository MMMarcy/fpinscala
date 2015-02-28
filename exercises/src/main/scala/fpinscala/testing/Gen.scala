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

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case res if res.isFalsified => p.run(max, n, rng)
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

  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp = forAll(listOf(smallInt)) { ls =>
    val sorted = ls.sorted;
    if (sorted.length == 0)
      true;
    else
      (0 until (sorted.length - 1)).forall(i => sorted(i) <= sorted(i + 1))
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
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

  def listOf[A](g: Gen[A]): Gen[List[A]] = {
    boolean.flatMap {
      b =>
        if (b) g.flatMap(a => listOf(g).map(l => a :: l))
        else unit(Nil)
    }
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf1[A](g: Gen[A]): Gen[List[A]] = {
    listOfN(1, g).flatMap(l1 => listOf(g).map(l2 => l1 ::: l2))
  }

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

  //Exercise 8.12
  def listOfN[A](gen: Gen[A]): SGen[List[A]] = SGen {
    i => listOfN(i, gen)
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

