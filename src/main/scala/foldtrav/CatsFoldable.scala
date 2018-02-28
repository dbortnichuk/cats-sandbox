package foldtrav

object CatsFoldable extends App {

  println(List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a))
  // res6: List[Int] = List(3, 2, 1)
  println(List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a))

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) :: accum
    }

  map(List(1, 2, 3))(_ * 2)

  // res9: List[Int] = List(2, 4, 6)
  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) ::: accum
    }

  flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))

  // res10: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)
  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (item, accum) =>
      if (func(item)) item :: accum else accum
    }

  filter(List(1, 2, 3))(_ % 2 == 1)

  // res11: List[Int] = List(1, 3)import scala.math.Numeric
  def sumWithNumeric[A](list: List[A])
                       (implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)

  sumWithNumeric(List(1, 2, 3))
  // res13: Int = 6

  import cats.Monoid
  def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)
  import cats.instances.int._ // for Monoid
  sumWithMonoid(List(1, 2, 3))
  // res16: Int = 6
}