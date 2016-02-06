package com.github.crockeo.booler

// Testing the functionality in src/main/scala/expression.scala.
class ExpressionTest extends org.scalatest.FunSuite {
  import com.github.crockeo.booler.Eval._

  // WARNING:
  //   I Know the unit tests don't really test a lot of the cases for the
  //   different Expression types, but I'm just kind of assuming that with their
  //   visual similarity to AND, I'd be able to debug a difference pretty
  //   quickly without extra unit testing.

  // Testing the truth tables of the different kinds of expressions.
  test("Expression.eval()") {
    assert(Bool(true).eval() == Some(true))
    assert(Bool(false).eval() == Some(false))

    assert(Var().eval() == None)

    assert(AND(Bool(false), Bool(false)).eval() == Some(false))
    assert(AND(Bool(false), Bool(true)).eval() == Some(false))
    assert(AND(Bool(true), Bool(false)).eval() == Some(false))
    assert(AND(Bool(true), Bool(true)).eval() == Some(true))

    assert(OR(Bool(false), Bool(false)).eval() == Some(false))
    assert(OR(Bool(false), Bool(true)).eval() == Some(true))
    assert(OR(Bool(true), Bool(false)).eval() == Some(true))
    assert(OR(Bool(true), Bool(true)).eval() == Some(true))

    assert(XOR(Bool(false), Bool(false)).eval() == Some(false))
    assert(XOR(Bool(false), Bool(true)).eval() == Some(true))
    assert(XOR(Bool(true), Bool(false)).eval() == Some(true))
    assert(XOR(Bool(true), Bool(true)).eval() == Some(false))

    assert(XOR(Bool(false), Bool(false)).eval() == Some(false))
    assert(XOR(Bool(false), Bool(true)).eval() == Some(true))
    assert(XOR(Bool(true), Bool(false)).eval() == Some(true))
    assert(XOR(Bool(true), Bool(true)).eval() == Some(false))

    assert(IF(Bool(false), Bool(false)).eval() == Some(true))
    assert(IF(Bool(false), Bool(true)).eval() == Some(true))
    assert(IF(Bool(true), Bool(false)).eval() == Some(false))
    assert(IF(Bool(true), Bool(true)).eval() == Some(true))

    assert(IFF(Bool(false), Bool(false)).eval() == Some(true))
    assert(IFF(Bool(false), Bool(true)).eval() == Some(false))
    assert(IFF(Bool(true), Bool(false)).eval() == Some(false))
    assert(IFF(Bool(true), Bool(true)).eval() == Some(true))
  }

  // Testing the functions within Eval.
  test("countVars") {
    assert(countVars(Bool(true)) == 0)
    assert(countVars(Var()) == 1)
    assert(countVars(AND(Var(), OR(Var(), XOR(Var(), IF(Var(), IFF(Var(), Var())))))) == 6)
  }

  test("fillVariable") {
    assert(fillVariable(true, AND(Bool(true), Bool(false))) == None)
    assert(fillVariable(true, AND(Var(), Var())) == Some(AND(Bool(true), Var())))
    assert(fillVariable(true, OR(Bool(true), Var())) == Some(OR(Bool(true), Bool(true))))
  }

  test("fillVariables") {
    assert(fillVariables(List(true, false, false), AND(Bool(false), AND(Var(), Bool(true)))) == None)
    assert(fillVariables(List(true, false, false), AND(Var(), AND(Var(), Var()))) == Some(AND(Bool(true), AND(Bool(false), Bool(false)))))
  }

  test("evaluateFilled") {
    assert(!evaluateFilled(List(true, true, false), AND(Var(), Bool(true))))
    assert(evaluateFilled(List(true, true, false), AND(Var(), OR(Var(), Var()))))
  }

  test("evaluateAll") {
    assert(!evaluateAll(List(true, true, false), AND(Var(), OR(Var(), Var()))))
    assert(evaluateAll(List(true, true), OR(Var(), Var())))
  }

  test("isSymmetric") {
    assert(isSymmetric(Var(), 1))
    assert(isSymmetric(AND(Var(), Var()), 2))
    assert(!isSymmetric(AND(Var(), Var()), 1))
  }
}
