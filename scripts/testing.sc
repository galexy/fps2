// import $file.state, state.*
// import $file.rng, rng.* 

type State[S, +A] = S => (A, S)

object State:
    def apply[S, A](f: S => (A, S)): State[S, A] = f

    def unit[S, A](a: A): State[S, A] =
        s => (a, s)

    def sequence[S, A](s: List[State[S, A]]): State[S, List[A]] =
        s.foldRight(unit(Nil : List[A]))((s, acc) => s.map2(acc)(_ :: _))

extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
        s => 
            val (a, s2) = underlying(s)
            f(a)(s2)
    
    def map[B](f: A => B): State[S, B] =
        underlying.flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        underlying.flatMap(a => sb.map(b => f(a, b)))

trait RNG:
    def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
        val newSeed = (seed * 0x5DeeCE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)

def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, nextRNG) = rng.nextInt
    val absN = if n == Int.MinValue then 0 else scala.math.abs(n)
    (n, nextRNG)

// ************

type TestCases = Int
object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

object Prop:
    type FailedCase = String
    type SuccessCount = Int

type Prop = (TestCases, RNG) => Result

enum Result:
    case Passed
    case Falsified(failure: Prop.FailedCase, successes: Prop.SuccessCount)

    def isFalsified: Boolean = this match
        case Passed => false
        case _ => false


type Gen[+A] = State[RNG, A]

extension [A](self: Gen[A])
    def listOfN(size: Gen[Int]): Gen[List[A]] =
        size.flatMap(n => State.sequence(List.fill(n)(self)))

    def product[B](other: Gen[B]): Gen[(A, B)] =
        self.flatMap(a => other.map(b => (a, b)))

    def **[B](other: Gen[B]): Gen[(A, B)] = product(other)

def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if _ then g1 else g2)

def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(nonNegativeInt).map(_ % (stopExclusive - start) + start)

def unit[A](a: => A): Gen[A] =
    State.unit(a)

def boolean: Gen[Boolean] =
    choose(0, 2).map(_ == 1)

def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    import Result.*

    (n, rng) =>
        randomLazyList(as)(rng)
            .zip(LazyList.from(0))
            .take(n)
            .map{
                case (a, i) =>
                    try
                        if f(a) then Passed
                        else Falsified(a.toString, i)
                    catch
                        case e: Exception =>
                            Falsified(buildMsg(a, e), i)
            }
            .find(_.isFalsified)
            .getOrElse(Passed)

def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

extension (self: Prop)
    def &&(that: Prop): Prop =
        (n, rng) => self(n, rng) match
            case Result.Passed => that(n, rng)
            case f => f

    def ||(that: Prop): Prop =
        (n, rng) => self(n, rng) match
            case Result.Passed => Result.Passed
            case _ => that(n, rng)

    def run(): Unit =
        self(100, SimpleRNG(System.currentTimeMillis)) match
            case Result.Falsified(msg, n) =>
                println(s"! Falsified after $n passed tests:\n $msg")
            case Result.Passed =>
                println(s"+ OK, passed 100 tests.")
