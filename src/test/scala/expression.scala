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

    assert(Var("test").eval() == None)

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

  test("varNames") {
    assert(varNames(Bool(true)) == Nil)
    assert(varNames(AND(Var("this is"), Var("cool"))) == List("this is", "cool"))
  }

  // Testing the functions within Eval.
  test("countVars") {
    assert(countVars(Bool(true)) == 0)
    assert(countVars(Var("test")) == 1)
    assert(countVars(AND(Var("test"), OR(Var("test"), XOR(Var("test"), IF(Var("test"), IFF(Var("test"), Var("test"))))))) == 6)
  }

  test("fillVariable") {
    assert(fillVariable("a", true, AND(Bool(true), Bool(false))) == AND(Bool(true), Bool(false)))
    assert(fillVariable("a", true, AND(Var("a"), Bool(false))) == AND(Bool(true), Bool(false)))
    assert(fillVariable("a", true, AND(Var("a"), OR(Var("b"), Var("a")))) ==
      AND(Bool(true), OR(Var("b"), Bool(true))))
  }

  test("fillVariables") {
    val e = AND(Var("a"), OR(Var("b"), XOR(Var("c"), Var("a"))))

    assert(fillVariables(Nil, e) == e)
    assert(fillVariables(List(("a", true)), e) == AND(Bool(true), OR(Var("b"), XOR(Var("c"), Bool(true)))))
    assert(fillVariables(List(("a", true), ("b", false)), e) == AND(Bool(true), OR(Bool(false), XOR(Var("c"), Bool(true)))))
  }

  test("evaluateFilled") {
    val e = AND(Var("a"), OR(Var("b"), XOR(Var("c"), Var("a"))))

    assert(evaluateFilled(List(("a", true), ("b", false)), e) == None)
    assert(evaluateFilled(List(("a", true), ("b", false), ("c", true)), e) == Some(false))
  }

  test("evaluateAll") {
    assert(evaluateAll(List("a", "b"), List(true, true), OR(Var("a"), Var("b"))))
    assert(evaluateAll(List("a", "b"), List(true, false), XOR(Var("a"), Var("b"))))
    assert(!evaluateAll(List("a", "b", "c"), List(true, true, false), AND(Var("a"), OR(Var("b"), Var("c")))))
  }

  test("isSymmetric") {
    assert(isSymmetric(Var("a"), 1))
    assert(isSymmetric(XOR(Var("a"), Var("b")), 1))
    assert(isSymmetric(AND(Var("a"), Var("b")), 2))
    assert(!isSymmetric(AND(Var("a"), Var("b")), 1))
  }
}
