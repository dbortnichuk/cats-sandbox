package monad

import cats.{Id, Monad}
import cats.instances.list._
import cats.instances.future._
import cats.instances.option._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.language.higherKinds

object CatsMonad extends App{

  val opt1 = Monad[Option].pure(3)
  // opt1: Option[Int] = Some(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  // opt2: Option[Int] = Some(5)
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)
  // opt3: Option[Int] = Some(500)
  val list1 = Monad[List].pure(3)
  // list1: List[Int] = List(3)
  val list2 = Monad[List].
    flatMap(List(1, 2, 3))(a => List(a, a * 10))
  // list2: List[Int] = List(1, 10, 2, 20, 3, 30)
  val list3 = Monad[List].map(list2)(a => a + 123)
  // list3: List[Int] = List(124, 133, 125, 143, 126, 153)


  Monad[Option].flatMap(Option(1))(a => Option(a * 2))
  // res0: Option[Int] = Some(2)
  import cats.instances.list._ // for Monad
  Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
  // res1: List[Int] = List(1, 10, 2, 20, 3, 30)
  import cats.instances.vector._ // for Monad
  Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))
  // res2: Vector[Int] = Vector(1, 10, 2, 20, 3, 30)


  val fm = Monad[Future]
  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  val r = Await.result(future, 1.second)

  //println(r)

  //def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x*x + y*y))

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  println(sumSquare(Option(3), Option(4)))

  println(sumSquare(List(1, 2, 3), List(4, 5)))

  println(sumSquare(3 : Id[Int], 4 : Id[Int]))

  println(sumSquare(List(1, 2, 3) : Id[List[Int]], List(4, 5) : Id[List[Int]]))

  val a = Monad[Id].pure(3)
  // a: cats.Id[Int] = 3
  val b = Monad[Id].flatMap(a)(_ + 1)
  // b: cats.Id[Int] = 4
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  for {
    x <- a
    y <- b
  } yield x + y
  // res6: cats.Id[Int] = 7

}
