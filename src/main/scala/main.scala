package com.github.crockeo.booler

object Main {
  // "symmetry" program mode.
  def symmetry(e: Expression): Unit = {
    println("Checking symmetry...")
    for (x <- 0.to(Eval.countVars(e)))
      println("Symmetric at " + x + ": " + (if (Eval.isSymmetric(e, x)) "Y"
                                            else                        "N"))
  }

  // The entry point to the program.
  def main(args: Array[String]): Unit = {
    if (args.length < 2)
      println("Proper usage: booler.jar <mode> <expression to parse>")
    else {
      val s = args.drop(1).mkString(" ")

      Parsing.expression(s) match {
        case None    => println("Failed to parse expression: " + s)
        case Some(e) => args(0) match {
          case "symmetry" => symmetry(e)
          case _          => println("Unrecognized mode "+ args(0) + " .")
        }
      }
    }
  }
}
