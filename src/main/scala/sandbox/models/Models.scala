package sandbox.models

object Models {

  final case class Cat(name: String, age: Int, color: String)
  case class Order(totalCost: Double, quantity: Double)
  final case class Box[A](value: A)

  case class User(name: String, age: Int)

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  sealed trait Tree[+A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

}
