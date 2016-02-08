package com.github.crockeo.booler

object Main {
  // The entry point to the program.
  def main(args: Array[String]): Unit = {
    if (args.length < 2)
      println("Proper usage: booler.jar <mode> <expression to parse>")
    else {
      val s = args.drop(1).mkString(" ")
      Parsing.expression(s) match {
        case None    => println("Failed to parse expression: " + s)
        case Some(e) => println("no.")
      }
    }
  }
}
