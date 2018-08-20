package com.interpreter

object NaiveAdt extends App {

  sealed trait Term
  case class Lit(x: Int) extends Term
  case class Add(x: Term, y: Term) extends Term

  def eval(term: Term): Int = term match {
    case Lit(x) => x
    case Add(x, y) => eval(x) + eval(y)
  }

  def show(term: Term): String = term match {
    case Lit(x) => x.toString
    case Add(x, y) => s"(${show(x)} + ${show(y)})"
  }

  val expression = Add(Add(Lit(10), Lit(5)), Lit(5))
  println(eval(expression))
  println(show(expression))

}
