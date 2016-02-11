package com.github.crockeo.booler

// The specific form of the Expressions.
sealed trait Expression {
  def eval(): Option[Boolean]
}

case class Bool(b: Boolean) extends Expression {
  def eval(): Option[Boolean] = Some(b)
}

case class Var(name: String) extends Expression {
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

case class NOT(e: Expression) extends Expression {
  def eval(): Option[Boolean] =
    e.eval().map(!_)
}

// Evaluation on Expressions.
object Eval {
  // Getting the list of variable names within an expression.
  def varNames(e: Expression): Set[String] =
    e match {
      case Bool(_)     => Set()
      case Var(name)   => Set(name)
      case AND(e1, e2) => varNames(e1) ++ varNames(e2)
      case OR (e1, e2) => varNames(e1) ++ varNames(e2)
      case XOR(e1, e2) => varNames(e1) ++ varNames(e2)
      case IF (e1, e2) => varNames(e1) ++ varNames(e2)
      case IFF(e1, e2) => varNames(e1) ++ varNames(e2)
      case NOT(e)      => varNames(e)
    }

  // Getting the number of variables in an expression.
  def countVars(e: Expression): Int =
    varNames(e).size

  // Filling all ocurrences of a variable with a given name with its
  // corresponding boolean value.
  def fillVariable(name: String, b: Boolean, e: Expression): Expression =
    e match {
      case Var(n) if (n == name) => Bool(b)
      case AND(e1, e2)           => AND(fillVariable(name, b, e1), fillVariable(name, b, e2))
      case OR (e1, e2)           => OR (fillVariable(name, b, e1), fillVariable(name, b, e2))
      case XOR(e1, e2)           => XOR(fillVariable(name, b, e1), fillVariable(name, b, e2))
      case IF (e1, e2)           => IF (fillVariable(name, b, e1), fillVariable(name, b, e2))
      case IFF(e1, e2)           => IFF(fillVariable(name, b, e1), fillVariable(name, b, e2))
      case NOT(e)                => NOT(fillVariable(name, b, e))
      case e                     => e
    }

  // Filling all variables defined in m within the 
  def fillVariables(bs: List[(String, Boolean)], e: Expression): Expression =
    bs.foldLeft(e)((e, p) => p match { case (name, b) => fillVariable(name, b, e) })

  // Evaluating a list of variables in an expression.
  def evaluateFilled(bs: List[(String, Boolean)], e: Expression): Option[Boolean] =
    fillVariables(bs, e).eval()

  // Conjoining the return values of a given expression filled with every
  // permutation of a list of Booleans.
  def evaluateAll(names: List[String], bs: List[Boolean], e: Expression): Boolean =
    bs.permutations.map(bs => evaluateFilled(names.zip(bs), e)).foldLeft(true)((b: Boolean, ob: Option[Boolean]) => ob match {
      case None     => false
      case Some(ob) => b && ob
    })

  // Checking if an expression is symmetric on a given N number of truthy
  // variables.
  def isSymmetric(e: Expression, n: Int): Boolean = {
    // Generating a list of bools of length n where k are truthy and n - k are
    // false.
    def genBools(truthy: Int, size: Int): List[Boolean] =
      0.until(size).map(_ < truthy).toList

    val names = varNames(e).toList
    val vars = names.length
    if (n > vars)
      false
    else {
      0.until(vars).map(x => evaluateAll(names, genBools(x, vars), e) == (x == n)).foldLeft(true)(_ && _)
    }
  }
}
