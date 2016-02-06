package com.github.crockeo.booler

// The specific form of the Expressions.
sealed trait Expression {
  def eval(): Option[Boolean]
}

case class Bool(b: Boolean) extends Expression {
  def eval(): Option[Boolean] = Some(b)
}

case class Var() extends Expression {
  def eval(): Option[Boolean] = None
}

case class AND(e1: Expression, e2: Expression) extends Expression {
  def eval(): Option[Boolean] =
    for {
      a <- e1.eval()
      b <- e2.eval()
    } yield a && b
}

case class OR (e1: Expression, e2: Expression) extends Expression {
  def eval(): Option[Boolean] =
    for {
      a <- e1.eval()
      b <- e2.eval()
    } yield a || b
}

case class XOR(e1: Expression, e2: Expression) extends Expression {
  def eval(): Option[Boolean] =
    for {
      a <- e1.eval()
      b <- e2.eval()
    } yield (a && !b) || (!a && b)
}

case class IF (e1: Expression, e2: Expression) extends Expression {
  def eval(): Option[Boolean] =
    for {
      a <- e1.eval()
      b <- e2.eval()
    } yield !a || b
}

case class IFF(e1: Expression, e2: Expression) extends Expression {
  def eval(): Option[Boolean] =
    for {
      a <- e1.eval()
      b <- e2.eval()
    } yield a == b
}

// Evaluation on Expressions.
object Eval {
  // Counting the number of variables in an expression.
  def countVars(e: Expression): Int =
    e match {
      case Bool(_)     => 0
      case Var()       => 1
      case AND(e1, e2) => countVars(e1) + countVars(e2)
      case OR (e1, e2) => countVars(e1) + countVars(e2)
      case XOR(e1, e2) => countVars(e1) + countVars(e2)
      case IF (e1, e2) => countVars(e1) + countVars(e2)
      case IFF(e1, e2) => countVars(e1) + countVars(e2)
    }

  // Filling a single variable in an expression.
  def fillVariable(b: Boolean, e: Expression): Option[Expression] = {
    def fillPair(f: (Expression, Expression) => Expression, e1: Expression, e2: Expression): Option[Expression] =
      (fillVariable(b, e1) match {
        case None => fillVariable(b, e2) match {
          case None    => None
          case Some(e) => Some(e1, e)
        }
        case Some(e) => Some(e, e2)
      }) match {
        case None           => None
        case Some((e1, e2)) => Some(f(e1, e2))
      }

    e match {
      case Bool(_)     => None
      case Var()       => Some(Bool(b))
      case AND(e1, e2) => fillPair(AND(_, _), e1, e2)
      case OR (e1, e2) => fillPair(OR (_, _), e1, e2)
      case XOR(e1, e2) => fillPair(XOR(_, _), e1, e2)
      case IF (e1, e2) => fillPair(IF (_, _), e1, e2)
      case IFF(e1, e2) => fillPair(IFF(_, _), e1, e2)
    }
  }

  // Filling the variables in an expression.
  def fillVariables(bs: List[Boolean], e: Expression): Option[Expression] =
    bs match {
      case Nil     => Some(e)
      case x :: xs => fillVariable(x, e).flatMap(fillVariables(xs, _))
    }

  // Evaluating a list of variables in an expression.
  def evaluateFilled(bs: List[Boolean], e: Expression): Boolean =
    fillVariables(bs, e) match {
      case None    => false
      case Some(e) =>
        e.eval() match {
          case None    => false
          case Some(b) => b
        }
    }

  // Conjoining the return values of a given expression filled with every
  // permutation of a list of Booleans.
  def evaluateAll(bs: List[Boolean], e: Expression): Boolean =
    bs.permutations.map(evaluateFilled(_, e)).fold(true)(_ && _)

  // Checking if an expression is symmetric on a given N number of truthy
  // variables.
  def isSymmetric(e: Expression, n: Int): Boolean = {
    // Generating a list of bools of length n where k are truthy and n - k are
    // false.
    def genBools(truthy: Int, size: Int): List[Boolean] =
      0.until(size).map(_ < truthy).toList

    val vars = countVars(e)
    if (vars > n)
      false
    else {
      0.until(vars).map(x => evaluateAll(genBools(x, vars), e) == (x == n)).foldLeft(true)(_ && _)
    }
  }
}
