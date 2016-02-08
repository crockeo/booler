package com.github.crockeo.booler

trait ParserException extends Exception { }

case class EOFException() extends ParserException
case class MatchException() extends ParserException

// A container around the StringStream.
class StringStream(cs: List[Char]) {
  // Constructing a StringStream from a String.
  def this(str: String) =
    this(str.toList)

  // Checking if the StringStream is out of characters.
  def eof(): Boolean =
    cs == Nil

  // Peeking at the next character in the list.
  def peek(): Char =
    cs match {
      case Nil    => throw EOFException()
      case x :: _ => x
    }

  // Consuming some piece of input and creating a new StringStream.
  def consume(): (Char, StringStream) =
    cs match {
      case Nil     => throw EOFException()
      case x :: xs => (x, new StringStream(xs))
    }
}

// The Parser trait.
trait Parser[T] {
  // A synonym for this Parser so that one can use it within the anonymous
  // Parser.
  private val self = this

  // Mapping a function over the Parser.
  def map[U](f: T => U): Parser[U] =
    new Parser[U] {
      def apply(s: StringStream): (U, StringStream) =
        self(s) match {
          case (t, s) => (f(t), s)
        }
    }

  // Monadic function mapping over a Parser.
  def flatMap[U](f: T => Parser[U]): Parser[U] =
    new Parser[U] {
      def apply(s: StringStream): (U, StringStream) =
        self(s) match {
          case (t, s) => f(t)(s)
        }
    }

  // Optionally matching this Parser.
  def ?(): Parser[Option[T]] =
    new Parser[Option[T]] {
      def apply(s: StringStream): (Option[T], StringStream) =
        try {
          self(s) match { case (t, s) => (Some(t), s) }
        } catch {
          case e: ParserException => (None, s)
          case e: Exception      => throw e
        }
    }

  // Sequencing this with a couple of different kinds of Parsers
  def ~[U](p: Parser[U]): Parser[(T, U)] = for {
    t <- self
    u <- p
  } yield (t, u)

  def $(p: Parser[List[T]]): Parser[List[T]] = for {
    t <- self
    ts <- p
  } yield t :: ts

  // Trying to either match this or another Parser.
  def |(p: Parser[T]): Parser[T] =
    new Parser[T] {
      def apply(s: StringStream): (T, StringStream) =
        self.?()(s) match {
          case (None   , _) => p(s)
          case (Some(t), s) => (t, s)
        }
    }

  // The main application of the Parser.
  def apply(s: StringStream): (T, StringStream)

  // Applying the Parser while throwing away the resulting StringStream.
  def !(s: StringStream): T =
    self(s)._1
}

object Parsers {
  // Some character predicates.
  def isWhitespace(c: Char): Boolean = 
    c == ' ' || c == '\t' || c == '\r' || c == '\n'

  def isAlpha(c: Char): Boolean =
    ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

  def isNum(c: Char): Boolean =
    '0' <= c && c <= '9'

  def isAlphaNum(c: Char): Boolean =
    isAlpha(c) || isNum(c)

  // A parse that matches nothing and returns Nil.
  def nil[T](): Parser[List[T]] =
    new Parser[List[T]] {
      def apply(s: StringStream) =
        (Nil, s)
    }

  // Matching a single character in a parser.
  def char(c: Char): Parser[Char] =
    new Parser[Char] {
      def apply(s: StringStream): (Char, StringStream) =
        s.consume match {
          case (a, s) if (c == a) => (a, s)
          case _                  => throw MatchException()
        }
    }

  // Matching against a list of characters.
  def chars(cs: List[Char]): Parser[List[Char]] =
    cs.map(char).reverse.foldLeft(nil[Char])((b: Parser[List[Char]], a: Parser[Char]) => a $ b)

  // Matching a string.
  def string(str: String): Parser[String] =
    chars(str.toList).map(_.mkString)

  // Matching a string over some predicate on a char.d
  def takeWhile(f: Char => Boolean): Parser[String] = {
    def takeWhileHelper(): Parser[List[Char]] =
      new Parser[List[Char]] {
        def apply(s: StringStream): (List[Char], StringStream) =
          if (s.eof || !f(s.peek))
            (Nil, s)
          else
            s.consume match {
              case (c, s) => apply(s) match {
                case (cs, s) => (c :: cs, s)
              }
            }
      }

    takeWhileHelper.map(_.mkString)
  }

  // A parser that consumes whitespace.
  def consumeWhitespace(): Parser[Unit] = for {
    _ <- takeWhile(isWhitespace)
  } yield Unit
}

// Parsing out Expressions from string input.
object Parsing {
  // Parsing out a boolean expression.
  val boolParser: Parser[Expression] = for {
    s <- Parsers.string("true") | Parsers.string("false")
  } yield s match {
    case "true"  => Bool(true)
    case "false" => Bool(false)
  }

  // Parsing out a variable expression.
  val varParser: Parser[Expression] =
    Parsers.takeWhile(Parsers.isAlpha).map(Var(_))

  // Parsing out any expression-based binary operand.
  def binaryExpression(name: String, f: (Expression, Expression) => Expression): Parser[Expression] = for {
    _ <- Parsers.string(name)
    _ <- Parsers.consumeWhitespace

    a <- expressionParser

    _ <- Parsers.consumeWhitespace

    b <- expressionParser
  } yield f(a, b)

  // Each of the binary operands for the language.
  val andParser = binaryExpression("AND", (a, b) => AND(a, b))
  val orParser  = binaryExpression("OR" , (a, b) => OR (a, b))
  val xorParser = binaryExpression("XOR", (a, b) => XOR(a, b))
  val ifParser  = binaryExpression("IF" , (a, b) => IF (a, b))
  val iffParser = binaryExpression("IFF", (a, b) => IFF(a, b))

  // The parser directly for parsing out an expression. varParser is at the end
  // because it would match most any input that the other Parsers would as well.
  val expressionParser: Parser[Expression] =
    boolParser | andParser | orParser | xorParser | ifParser | iffParser | varParser

  // Trying to parse out an Expression from a string.
  def expression(str: String): Option[Expression] =
    expressionParser.?.!(new StringStream(str))
}
