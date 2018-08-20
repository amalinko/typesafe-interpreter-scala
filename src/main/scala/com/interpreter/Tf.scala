package com.interpreter
import com.interpreter.Tf.Term.{Eval, StringView, TreeView}

import scala.language.higherKinds

object Tf extends App {

  trait Term[F[_]] {

    def lit(x: Int): F[Int]

    def add(x: F[Int], y: F[Int]): F[Int]

    def gt(x: F[Int], y: F[Int]): F[Boolean]

    def ifElse[A](condition: F[Boolean], x: F[A], y: F[A]): F[A]

  }

  object Term {

    case class Eval[A](value: A)

    implicit val evaluator: Term[Eval] = new Term[Eval] {

      override def lit(x: Int): Eval[Int] = Eval(x)

      override def add(x: Eval[Int], y: Eval[Int]): Eval[Int] = Eval(x.value + y.value)

      override def ifElse[A](condition: Eval[Boolean], x: Eval[A], y: Eval[A]): Eval[A] =
        if (condition.value) Eval(x.value) else Eval(y.value)

      override def gt(x: Eval[Int], y: Eval[Int]): Eval[Boolean] = Eval(x.value > y.value)

    }

    case class StringView[A](value: String)

    implicit val stringSerializer: Term[StringView] = new Term[StringView] {

      override def lit(x: Int): StringView[Int] = StringView(x.toString)

      override def add(x: StringView[Int], y: StringView[Int]): StringView[Int] =
        StringView(s"(${x.value} + ${y.value})")

      override def gt(x: StringView[Int], y: StringView[Int]): StringView[Boolean] =
        StringView(s"${x.value} > ${y.value}")

      override def ifElse[A](condition: StringView[Boolean], x: StringView[A], y: StringView[A]): StringView[A] =
        StringView(s"if(${condition.value}) ${x.value} else ${y.value}")

    }

    case class TreeView[A](value: Tree)

    implicit val treeSerializer: Term[TreeView] = new Term[TreeView] {

      override def lit(x: Int): TreeView[Int] = TreeView(Node("Lit", List(Leaf(x.toString))))

      override def add(x: TreeView[Int], y: TreeView[Int]): TreeView[Int] =
        TreeView(Node("Add", List(x.value, y.value)))

      override def gt(x: TreeView[Int], y: TreeView[Int]): TreeView[Boolean] =
        TreeView(Node("Gt", List(x.value, y.value)))

      override def ifElse[A](condition: TreeView[Boolean], x: TreeView[A], y: TreeView[A]): TreeView[A] =
        TreeView(Node("IfElse", List(condition.value, x.value, y.value)))

    }

  }

  def expression[F[_]](x: Int)(implicit T: Term[F]): F[Int] =
    T.ifElse(T.gt(T.lit(x), T.lit(5)), T.lit(x), T.add(T.add(T.lit(x), T.lit(10)), T.lit(5)))

  println(expression[Eval](3).value)
  println(expression[StringView](3).value)
  println(expression[TreeView](3).value)
}
