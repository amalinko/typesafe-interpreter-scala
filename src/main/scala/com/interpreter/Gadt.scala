package com.interpreter

object Gadt extends App {

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

  def serializeToString[_](term: Term[_]): String = term match {
    case Lit(x) =>
      x.toString
    case Add(x, y) =>
      s"(${serializeToString(x)} + ${serializeToString(y)})"
    case Gt(x, y) =>
      s"${serializeToString(x)} > ${serializeToString(y)}"
    case IfElse(condition, x, y) =>
      s"if(${serializeToString(condition)}) ${serializeToString(x)} else ${serializeToString(y)}"
  }

  def serializeToTree[_](term: Term[_]): Tree = term match {
    case Lit(x) =>
      Node("Lit", List(Leaf(x.toString)))
    case Add(x, y) =>
      Node("Add", List(serializeToTree(x), serializeToTree(y)))
    case Gt(x, y) =>
      Node("Gt", List(serializeToTree(x), serializeToTree(y)))
    case IfElse(condition, x, y) =>
      Node("IfElse", List(serializeToTree(condition), serializeToTree(x), serializeToTree(y)))
  }

  val expression = IfElse(Gt(Add(Lit(5), Lit(6)), Lit(5)), Add(Add(Lit(11), Lit(5)), Lit(5)), Lit(2))
  println(eval(expression))
  println(serializeToString(expression))
  println(serializeToTree(expression))

}
