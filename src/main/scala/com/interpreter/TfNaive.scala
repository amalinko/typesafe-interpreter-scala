package com.interpreter

object TfNaive extends App {

  trait Term[A] {
    def lit(x: Int): A
    def add(x: A, y: A): A
  }

  object Term {
    implicit val evaluator: Term[Int] = new Term[Int] {
      override def lit(x: Int): Int = x
      override def add(x: Int, y: Int): Int = x + y
    }
    implicit val printer: Term[String] = new Term[String] {
      override def lit(x: Int): String = x.toString
      override def add(x: String, y: String): String = s"($x + $y)"
    }
  }

  def addTenAndFive[A](x: Int)(implicit T: Term[A]): A = T.add(T.add(T.lit(x), T.lit(10)), T.lit(5))

  println(addTenAndFive[String](10))
  println(addTenAndFive[Int](10))

}
