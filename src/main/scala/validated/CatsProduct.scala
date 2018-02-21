package validated

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._ // for map
import cats.instances.list._ // for Semigroupal

import cats.instances.either._

object CatsProduct extends App {

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = x.flatMap(a => y.map(b => (a, b)))

  def product1[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)

  println(product(List(1, 2), List(3, 4)))
  // res12: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))
  type ErrorOr[A] = Either[Vector[String], A]
  println{product[ErrorOr, Int, Int](
    Left(Vector("Error 1")),
    Left(Vector("Error 2"))
  )}
  // res13: ErrorOr[(Int, Int)] = Left(Vector(Error 1))

}
