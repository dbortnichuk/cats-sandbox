package sandbox.models

object Models {

  final case class Cat(name: String, age: Int, color: String)
  case class Order(totalCost: Double, quantity: Double)
  final case class Box[A](value: A)

}
