package com.interpreter

import scala.util.Try

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

  trait GadtTyping[A] {
    def intResult(x: Term[Int]): Either[String, Term[A]]
    def boolResult(x: Term[Boolean]): Either[String, Term[A]]
  }

  object GadtTyping {
    implicit val intTyping: GadtTyping[Int] = new GadtTyping[Int] {
      override def intResult(x: Term[Int]): Either[String, Term[Int]] = Right(x)
      override def boolResult(x: Term[Boolean]): Either[String, Term[Int]] = Left("Type error")
    }
    implicit val boolTyping: GadtTyping[Boolean] = new GadtTyping[Boolean] {
      override def intResult(x: Term[Int]): Either[String, Term[Boolean]] = Left("Type error")
      override def boolResult(x: Term[Boolean]): Either[String, Term[Boolean]] = Right(x)
    }
  }

  def fromTree[A](tree: Tree)(implicit t: GadtTyping[A]): Either[String, Term[A]] = tree match {
    case Node("Lit", Leaf(value) :: Nil) =>
      parseInt(value).flatMap(x => t.intResult(Lit.apply(x)))
    case Node("Add", xLeaf :: yLeaf :: Nil) =>
      for {
        x <- fromTree[Int](xLeaf)
        y <- fromTree[Int](yLeaf)
        res <- t.intResult(Add(x, y))
      } yield res
    case Node("Gt", xLeaf :: yLeaf :: Nil) =>
      for {
        x <- fromTree[Int](xLeaf)
        y <- fromTree[Int](yLeaf)
        res <- t.boolResult(Gt(x, y))
      } yield res
    case Node("IfElse", cLeaf :: xLeaf :: yLeaf :: Nil) =>
      for {
        c <- fromTree[Boolean](cLeaf)
        x <- fromTree[Int](xLeaf)
        y <- fromTree[Int](yLeaf)
        res <- t.intResult(IfElse(c, x, y))
      } yield res
    case other =>
      Left(s"Unable to parse: $other")
  }

  private def parseInt(x: String): Either[String, Int] = Try(x.toInt).toOption.toRight(s"Unable to parse $x")

  val expression = IfElse(Gt(Add(Lit(5), Lit(6)), Lit(5)), Add(Add(Lit(11), Lit(5)), Lit(5)), Lit(2))
  println(eval(expression))
  println(serializeToString(expression))
  val tree: Tree = serializeToTree(expression)
  println(tree)
  val deserializedExpression = fromTree[Int](tree).getOrElse(throw new Exception)
  println(eval(deserializedExpression))

}
