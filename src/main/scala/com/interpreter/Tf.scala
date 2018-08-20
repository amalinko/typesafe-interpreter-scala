package com.interpreter
import com.interpreter.Tf.Term.{Eval, View}

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

    implicit val intEvaluator: Term[Eval] = new Term[Eval] {

      override def lit(x: Int): Eval[Int] = Eval(x)

      override def add(x: Eval[Int], y: Eval[Int]): Eval[Int] = Eval(x.value + y.value)

      override def ifElse[A](condition: Eval[Boolean], x: Eval[A], y: Eval[A]): Eval[A] =
        if (condition.value) Eval(x.value) else Eval(y.value)

      override def gt(x: Eval[Int], y: Eval[Int]): Eval[Boolean] = Eval(x.value > y.value)

    }

    case class View[A](value: String)

    implicit val printer: Term[View] = new Term[View] {

      override def lit(x: Int): View[Int] = View(x.toString)

      override def add(x: View[Int], y: View[Int]): View[Int] = View(s"(${x.value} + ${y.value})")

      override def gt(x: View[Int], y: View[Int]): View[Boolean] = View(s"${x.value} > ${y.value}")

      override def ifElse[A](condition: View[Boolean], x: View[A], y: View[A]): View[A] =
        View(s"if(${condition.value}) ${x.value} else ${y.value}")

    }

  }

  def addTenAndFive[F[_]](x: Int)(implicit T: Term[F]): F[Int] = {
    T.ifElse(T.gt(T.lit(x), T.lit(5)), T.lit(x), T.add(T.add(T.lit(x), T.lit(10)), T.lit(5)))
  }

  println(addTenAndFive[Eval](3).value)
  println(addTenAndFive[View](3).value)
}
