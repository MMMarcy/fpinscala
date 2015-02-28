package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def nOfConsecutiveAs(s: String): Parser[Int]
  def nOfChars(s: String, c: Char): Parser[Int]


  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

  object Laws {
    def runString(s: String) = run(string(s))(s) == Right(s)
    def runChar(c: Char) =   run(char(c))(c.toString) == Right(c)
    def runConssectuvieAs(s: String, expected: Int) = run(nOfConsecutiveAs(s))(s) == Right(expected)
    def runNofChars(s: String, c: Char)( expected: Int) = run(nOfChars(s,c))(s) == Right(expected)
    def emptyStringLaw[A]( p: Parser[A]) = run(p)("") == Left("Trying to parse empty")


    val t1 = runConssectuvieAs("aa",2)
    val t2 = runConssectuvieAs("b123",0)
    val t3 = runConssectuvieAs("baa",0)

    val t4 = runNofChars("bab", 'a')(1)
    val t5 = runNofChars("bab", 'b')(2)
    val t6 = runNofChars("bab", 'c')(0)

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
  otherFailures: List[ParseError] = List()) {
}