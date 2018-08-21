package com.interpreter

// https://gist.github.com/Odomontois/b107e439b28795e9edd912f4b572443b
object kiselyovTagless extends App {

  //strict DSL language for arithmetic operations
  trait Expr[Repr] {
    def lit(v: Int): Repr
    def neg(a: Repr): Repr
    def add(a: Repr, b: Repr): Repr
  }
  //interpreters
  implicit val expSymInt: Expr[Int] = new Expr[Int] {
    override def lit(v: Int)         = v
    override def neg(a: Int)         = -a
    override def add(a: Int, b: Int) = a + b
  }

  implicit val expSymString: Expr[String] = new Expr[String] {
    override def lit(v: Int): String               = v.toString
    override def neg(a: String): String            = s"(-$a)"
    override def add(a: String, b: String): String = s"($a+$b)"
  }

  //The sample example of program parameterised by a resulted type
  def expr1[Repr](implicit i: Expr[Repr]) =
    i.add(i.lit(8), i.neg(i.add(i.lit(1), i.lit(2))))

  //Serialisation:
  //JSON-like serialisation format
  sealed trait Tree
  final case class Leaf(v: String)                 extends Tree
  final case class Node(v: String, ts: List[Tree]) extends Tree

  //serializer-interpreter:
  implicit val expSymTree: Expr[Tree] = new Expr[Tree] {
    override def lit(v: Int)           = Node("Literal", List(Leaf(v.toString)))
    override def neg(a: Tree)          = Node("Negation", List(a))
    override def add(a: Tree, b: Tree) = Node("Addition", List(a, b))
  }

  //Substitution of different interpreters
  expr1[Int]    // 5: Int
  expr1[String] // (8+(-(1+2))): String
  val tree = expr1[Tree]
  //Node(Addition,List(
  //  Node(Literal,List(Leaf(8))),
  //  Node(Negation,List(
  //    Node(Addition,List(
  //      Node(Literal,List(Leaf(1))),
  //      Node(Literal,List(Leaf(2))))))))): Tree

  //Deserialization:
  //The input "Tree" structure may be invalid, so we need to handle errors
  //We'll use the "Either" for this
  type ErrMsg = String
  def safeReadInt(s: String): Either[ErrMsg, Int] = {
    import scala.util.Try
    Try(s.toInt).toEither.left.map(_ => s"Couldn't parse to Int: '$s'")
  }

  //Here we want to convert a Tree directly into a result of a certain interpreter
  //The 'A' type parameter is used for interpreter substitution
  def fromTree[A](tree: Tree)(implicit e: Expr[A]): Either[ErrMsg, A] = tree match {
    case Node("Literal", List(Leaf(n))) =>
      safeReadInt(n).map(e.lit)
    case Node("Negation", List(subTree)) =>
      fromTree(subTree)(e).map(e.neg)
    case Node("Addition", List(leftSubTree, rightSubTree)) =>
      for (lt <- fromTree(leftSubTree)(e); rt <- fromTree(rightSubTree)(e))
        yield e.add(lt, rt)
    case _ =>
      Left("Invalid tree")
  }

  println(fromTree[Int](tree))    // Right(5): Either[ErrMsg, Int]
  println(fromTree[String](tree)) // Right((8+(-(1+2)))): Either[ErrMsg,String]

  //The problem: We can not interpret the result of "fromTree(tree)" multiple times using various intepreters.
  //Currently, there is one-to-one connection

  //Solution (from the paper): Introduce the "Wrapped" type and rewrite the "fromTree" function to return it

  //Is it right?
  trait Wrapped {
    def apply[Repr: Expr]: Repr
  }

  object Wrapped {
    def mk: MkWrapped = new MkWrapped(true)

    final class MkWrapped(val dummy: Boolean = true) extends AnyVal {
      type A

      def apply(f: Expr[A] => A): Wrapped =
        new Wrapped {
          override def apply[Repr](implicit expr: Expr[Repr]): Repr =
            f(expr.asInstanceOf[Expr[A]]).asInstanceOf[Repr]
        }
    }
  }

  //How to implement this function?
  def fromTreeWrapped(tree: Tree): Either[ErrMsg, Wrapped] =
    tree match {
      case Node("Literal", List(Leaf(n))) =>
        safeReadInt(n).right.map(s => Wrapped.mk(e => e.lit(s)))
      case Node("Negation", List(subTree)) =>
        fromTreeWrapped(subTree).map(w => Wrapped.mk(e => e.neg(w(e))))
      case Node("Addition", List(leftSubTree, rightSubTree)) =>
        for (lt <- fromTreeWrapped(leftSubTree); rt <- fromTreeWrapped(rightSubTree))
          yield Wrapped.mk(e => e.add(lt(e), rt(e)))
      case _ =>
        Left("Invalid tree")
    }
}
