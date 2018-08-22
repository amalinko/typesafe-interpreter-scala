package com.interpreter
import com.interpreter.Tf.Term.{Eval, StringView, TreeView}

import scala.language.higherKinds
import scala.util.Try

object Tf extends App {

  trait Term[F[_]] {

    def lit(x: Int): F[Int]

    def add(x: F[Int], y: F[Int]): F[Int]

    def gt(x: F[Int], y: F[Int]): F[Boolean]

    def ifElse[A](condition: F[Boolean], x: F[A], y: F[A]): F[A]

    def func[A, B](a: F[A], f: F[A] => F[B]): F[B]

    def app[A, B](f: F[A] => F[B]): F[A => B]

  }

  object Term {

    case class Eval[A](value: A)

    implicit val evaluator: Term[Eval] = new Term[Eval] {

      override def lit(x: Int): Eval[Int] = Eval(x)

      override def add(x: Eval[Int], y: Eval[Int]): Eval[Int] = Eval(x.value + y.value)

      override def ifElse[A](condition: Eval[Boolean], x: Eval[A], y: Eval[A]): Eval[A] =
        if (condition.value) Eval(x.value) else Eval(y.value)

      override def gt(x: Eval[Int], y: Eval[Int]): Eval[Boolean] = Eval(x.value > y.value)

      override def func[A, B](a: Eval[A], f: Eval[A] => Eval[B]): Eval[B] = Eval(f(a).value)

      override def app[A, B](f: Eval[A] => Eval[B]): Eval[A => B] = Eval((a: A) => f(Eval(a)).value)
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

      override def func[A, B](a: StringView[A], f: StringView[A] => StringView[B]): StringView[B] = ???

      override def app[A, B](f: StringView[A] => StringView[B]): StringView[A => B] = ???
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

      override def func[A, B](a: TreeView[A], f: TreeView[A] => TreeView[B]): TreeView[B] = ???

      override def app[A, B](f: TreeView[A] => TreeView[B]): TreeView[A => B] = ???
    }

    trait TermTyping[F[_], A] {
      def fromBool(x: F[Boolean]): Either[String, F[A]]
      def fromInt(x: F[Int]): Either[String, F[A]]
    }

    object TermTyping {
      implicit def boolResult[F[_]]: TermTyping[F, Boolean] = new TermTyping[F, Boolean] {
        override def fromBool(x: F[Boolean]) = Right(x)
        override def fromInt(x: F[Int]) = Left("Type Error. Expected: Bool, got: Int")
      }

      implicit def intResult[F[_]]: TermTyping[F, Int] = new TermTyping[F, Int] {
        override def fromBool(x: F[Boolean]) = Left("Type Error. Expected: Int, got: Bool")
        override def fromInt(x: F[Int]) = Right(x)
      }
    }

    def fromTree[F[_], A](tree: Tree)(implicit T: Term[F], tt: TermTyping[F, A]): Either[String, F[A]] = tree match {
      case Node("Lit", Leaf(value) :: Nil) =>
        for {
          xi <- parseInt(value)
          res <- tt.fromInt(T.lit(xi))
        } yield res
      case Node("Add", xLeaf :: yLeaf :: Nil) =>
        for {
          x <- fromTree[F, Int](xLeaf)
          y <- fromTree[F, Int](yLeaf)
          res <- tt.fromInt(T.add(x, y))
        } yield res
      case Node("Gt", xLeaf :: yLeaf :: Nil) =>
        for {
          x <- fromTree[F, Int](xLeaf)
          y <- fromTree[F, Int](yLeaf)
          res <- tt.fromBool(T.gt(x, y))
        } yield res
      case Node("IfElse", cLeaf :: xLeaf :: yLeaf :: Nil) =>
        for {
          c <- fromTree[F, Boolean](cLeaf)
          x <- fromTree[F, Int](xLeaf)
          y <- fromTree[F, Int](yLeaf)
          res <- tt.fromInt(T.ifElse(c, x, y))
        } yield res
      case other => Left(s"Unable to parse: $other")
    }

    private def parseInt(x: String): Either[String, Int] = Try(x.toInt).toOption.toRight(s"Unable to parse $x")

  }

  def expression[F[_]](x: Int)(implicit T: Term[F]): F[Int] =
    T.ifElse(T.gt(T.lit(x), T.lit(5)), T.lit(x), T.add(T.add(T.lit(x), T.lit(10)), T.lit(5)))

  def fExp[F[_]](implicit T: Term[F]): F[Int => Int] = T.app((v: F[Int]) => T.add(v, T.lit(10)))


  val l = fExp[Eval]
  println(l.value(3))

  println(expression[Eval](3).value)
  println(expression[StringView](3).value)
  val tree: Tree = expression[TreeView](3).value
  println(tree)

  println(Term.fromTree[Eval, Int](tree))
}
