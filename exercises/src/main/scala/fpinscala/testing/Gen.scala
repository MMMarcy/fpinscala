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

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

}

/*trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???
  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}*/

trait SGen[+A] {

}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
  	size.flatMap(n => Gen.listOfN(n,this))
  }
    

}

