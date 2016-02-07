package com.github.crockeo.booler

// Testing the general StringStream class.
class StringStreamTest extends org.scalatest.FunSuite {
  test("StringStream.eof") {
    assert(new StringStream("").eof)
    assert(!new StringStream("test").eof)
  }

  test("StringStream.peek") {
    val s = new StringStream("ab")

    assert(s.peek == 'a')
    assert(s.peek == 'a')
  }

  test("StringStream.consume") {
    new StringStream("abc").consume match {
      case (a, s) => {
        assert(a == 'a')
        s.consume match {
          case (b, s) => {
            assert(s.peek == 'c')
            assert(b == 'b')
            s.consume match {
              case (c, s) => {
                assert(c == 'c')
                assert(s.eof)
              }
            }
          }
        }
      }
    }
  }
}

// Testing the functionality within parser.scala in the main directory.
class ParserTest extends org.scalatest.FunSuite {
  val s = new StringStream("This is a test")

  test("Parsers.char") {
    intercept[MatchException] {
      Parsers.char('a').!(s)
    }

    assert(Parsers.char('T').!(s) == 'T')
  }

  test("Parsers.chars") {
    intercept[MatchException] {
      Parsers.chars(List('T', 'h', 'i', 'd')).!(s)
    }

    assert(Parsers.chars(List('T', 'h', 'i', 's')).!(s) == List('T', 'h', 'i', 's'))
  }

  test("Parsers.string") {
    intercept[MatchException] {
      Parsers.string("Thid").!(s)
    }

    assert(Parsers.string("This is a test").!(s) == "This is a test")
  }

  test("Parsing.bool") {
    intercept[MatchException] {
      Parsing.boolParser.!(new StringStream("abcd"))
    }

    assert(Parsing.boolParser.!(new StringStream("true")) == Bool(true))
    assert(Parsing.boolParser.!(new StringStream("false")) == Bool(false))
  }

  test("Parsing.var") {
    assert(Parsing.varParser.!(new StringStream("testvar")) == Var("testvar"))
    assert(Parsing.varParser.!(new StringStream("split up")) == Var("split"))
    assert((for {
      v1 <- Parsing.varParser
      _  <- Parsers.consumeWhitespace
      v2 <- Parsing.varParser
    } yield (v1, v2)).!(new StringStream("split up")) == (Var("split"), Var("up")))
  }

  test("Parsing.binaries") {
    // Parsing all of the different binary operands.
    assert(Parsing.andParser.!(new StringStream("AND true false")) == AND(Bool(true), Bool(false)))
    assert(Parsing.orParser.! (new StringStream("OR  true false")) == OR (Bool(true), Bool(false)))
    assert(Parsing.xorParser.!(new StringStream("XOR true false")) == XOR(Bool(true), Bool(false)))
    assert(Parsing.ifParser.! (new StringStream("IF  true false")) == IF (Bool(true), Bool(false)))
    assert(Parsing.iffParser.!(new StringStream("IFF true false")) == IFF(Bool(true), Bool(false)))
  }

  test("Parsing.expressionParser") {
    assert(Parsing.expressionParser.!(new StringStream("AND XOR true p IF q OR u v")) == AND(XOR(Bool(true), Var("p")), IF(Var("q"), OR(Var("u"), Var("v")))))
  }
}
