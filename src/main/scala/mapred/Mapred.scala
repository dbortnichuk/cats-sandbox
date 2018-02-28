package mapred

import cats.Monoid
import cats.instances.int._
import cats.instances.string._
import cats.syntax.semigroup._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Mapred extends App{
//  def foldMap[A, B : Monoid](as: Vector[A])(func: A => B): B =
//    as.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  def foldMap[A, B : Monoid](as: Vector[A])(func: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| func(_))

//  println(foldMap(Vector(1, 2, 3))(identity))
//  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
//  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

  import cats.{Monad, Monoid}
  import cats.instances.int._ // for Monoid
  import cats.instances.future._ // for Monad and Monoid
  Monad[Future].pure(42)
  Monoid[Future[Int]].combine(Future(1), Future(2))

  def parallelFoldMap[A, B: Monoid]
  (values: Vector[A])
  (func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups: Iterator[Vector[A]] =
      values.grouped(groupSize)
    val futures: Iterator[Future[B]] =
      groups.map(group => Future(foldMap(group)(func)))
    Future.sequence(futures) map { iterable =>
      iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))

}
