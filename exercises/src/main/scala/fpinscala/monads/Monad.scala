package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }

}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    lma.foldRight(unit(Nil: List[A])) { (el, acc) =>
      map2(el, acc)(_ :: _)
    }
  }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List[B]())) { (el, acc) =>
      map2(f(el), acc)(_ :: _)
    }
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    @annotation.tailrec
    def go(n: Int, acc: M[List[A]]): M[List[A]] = {
      if (n == 0)
        acc
      else
        go(n - 1, map2(ma, acc)(_ :: _))
    }
    go(n, unit(List[A]()))
  }

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    def go(ms: List[A], acc: M[List[A]]): M[List[A]] = {
      ms match {
        case Nil => acc
        case (x :: xs) =>
          val newAcc = map2(f(x), acc)((bool, res) => if (bool) (x :: res) else res)
          go(xs, newAcc)
      }
    }
    go(ms, unit(List[A]()))
  }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => {
    flatMap(f(a))(b => g(b))
  }

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    compose((_: Unit) => ma, f)(())
  }

  def join[A](mma: M[M[A]]): M[A] = {
    flatMap(mma)(a => a)
  }

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    join(map(ma)(f))
  }
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.lazyUnit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      ma flatMap f
  }

  //TODO: finish chapter on parsers :/
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = {
      a flatMap f
    }

  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    override def flatMap[A, B](a: Stream[A])(f: A => Stream[B]): Stream[B] = {
      a flatMap f
    }
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A) = List(a)
    override def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = {
      a flatMap f
    }
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  lazy val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = {
      a.flatMap(f)
    }
  }

  def readerMonad[R] = Reader.readerMonad[R]
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = {
    f(value)
  }
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(r => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader{
      r =>  f(st.run.apply(r)).run(r)
    }
  }
}

