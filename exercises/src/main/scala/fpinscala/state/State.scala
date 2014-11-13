package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //Exercie 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, s1) = rng.nextInt;
    if (x == Int.MinValue)
      nonNegativeInt(s1)
    else
      (Math.abs(x), s1)
  }

  //Exrcise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (x, s1) = nonNegativeInt(rng)
    x.toDouble / Int.MaxValue -> s1
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x1, s1) = double(rng)
    val (x2, s2) = s1.nextInt
    (x2 -> x1) -> s2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), s1) = intDouble(rng)
    (d, i) -> s1
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    (d1, d2, d3) -> s3
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int)(rng: RNG)(acc: List[Int]): (List[Int], RNG) = {
      if (count == 0)
        acc -> rng
      else {
        val (x, s1) = rng.nextInt
        go(count - 1)(s1)(x :: acc)
      }
    }
    go(count)(rng)(Nil)
  }

  //Exercise 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  //Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (xa, s1) = ra(rng)
    val (xb, s2) = rb(s1)
    (f(xa, xb), s2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)(_ -> _)

  //Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f,acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
