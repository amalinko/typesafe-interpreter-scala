package com.interpreter

object TypedAdt extends App {

  sealed trait Term[A]
  case class Lit(x: Int) extends Term[Int]
  case class Add(x: Term[Int], y: Term[Int]) extends Term[Int]
  case class Gt(x: Term[Int], y: Term[Int]) extends Term[Boolean]
  case class IfElse[A](condition: Term[Boolean], x: Term[A], y: Term[A]) extends Term[A]

  def eval[A](term: Term[A]): A = term match {
    case Lit(x) => x
    case Add(x, y) => eval(x) + eval(y)
    case Gt(x, y) => eval(x) > eval(y)
    case IfElse(condition, x, y) => if (eval(condition)) eval(x) else eval(y)
  }

  val expression = IfElse(Gt(Add(Lit(5), Lit(6)), Lit(5)), Add(Add(Lit(11), Lit(5)), Lit(5)), Lit(2))
  val result = eval(expression)
  println(result)

}
