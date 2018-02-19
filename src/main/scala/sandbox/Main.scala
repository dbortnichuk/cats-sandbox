package sandbox

import cats.syntax.show._
import sandbox.ShowEx._
import sandbox.models.Models.{Cat, Order}
import sandbox.printable.Printable
import sandbox.printable.PrintableInstances._
import sandbox.printable.PrintableSyntax._
import cats.syntax.eq._
import cats.instances.option._
import monoid.Monoids._
import monoid.Adder._

object Main extends App {
  //println("Hello " |+| "Cats!")

  //Printable.print(5)

  val arny = Cat("Arny", 3, "Red")
  val sly = Cat("Sly", 3, "Blue")
  val bruce = Cat("Bruce", 2, "White")


  Printable.print(arny)

  sly.print

  println(bruce.show)

  println("Eq1 " + (arny === arny))
  println("Eq2 " +  (arny === sly))

  println("Eq3 " + (Option(bruce) === Option(bruce)))
  println("Eq4 " +  (Option(bruce) === None))

//  println("Eq5 " + (Some(bruce) =!= None))
//  println("Eq6 " + (Some(bruce) === Some(arny)))

  println("Eq5 " + ((Some(bruce) : Option[Cat]) =!= (None : Option[Cat])))
  println("Eq6 " + ((Some(bruce) : Option[Cat]) === (Some(arny) : Option[Cat])))

  println("intSetMonoid " + intSetMonoid.combine(Set(1, 2), Set(2, 3)))

  import cats.instances.int._ // for Monoid
  println("add ints " + add(List(1, 2, 3)))
  // res9: Int = 6
  import cats.instances.option._ // for Monoid
  println("add options " + add(List(Some(1), None, Some(2), None, Some(3))))

  println("add orders " + add(List(Order(1, 2), Order(2, 2), Order(3, 1))))

}
