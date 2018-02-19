package monoid

import cats.Monoid
import cats.instances.int._ // for Monoid
import cats.syntax.semigroup._ // for |+|
import sandbox.models.Models.Order


object Adder {



  def add(items: List[Int]): Int = items.foldLeft(Monoid[Int].empty)(_ |+| _)

//  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(monoid.empty)(_ |+| _)

  def add[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _) //same as above but with context bound

  implicit val monoid: Monoid[Order] = new Monoid[Order] {
    def combine(o1: Order, o2: Order) =
      Order(
        o1.totalCost + o2.totalCost,
        o1.quantity + o2.quantity
      )
    def empty = Order(0, 0)
  }

}
