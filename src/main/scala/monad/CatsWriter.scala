package monad

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._ // for Monoid

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object CatsWriter extends App {

  //  type Logged[A] = Writer[Vector[String], A]
  //
  //  123.pure[Logged]
  //
  //  Writer(Vector(
  //    "It was the best of times",
  //    "it was the worst of times"
  //  ), 1859)
  //
  //  Vector("msg1", "msg2", "msg3").tell
  //
  //  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  //
  //  println(a)
  //
  //  val b = 123.writer(Vector("msg1", "msg2", "msg3"))
  //
  //  println(b)
  //
  //  val aResult: Int = a.value
  //
  //  val aLog: Vector[String] = a.written
  //
  //  val (log, result) = b.run
  //
  //  val writer1 = for {
  //    a <- 10.pure[Logged]
  //    _ <- Vector("a", "b", "c").tell
  //    b <- 32.writer(Vector("x", "y", "z"))
  //  } yield a + b
  //
  //  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  //  println(writer2.run)


  type Logged[A] = Writer[Vector[String], A]
  42.pure[Logged]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

//  def factorial(n: Int): Int = {
//    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
//    println(s"fact $n $ans")
//    ans
//  }

  //factorial(5)

//  Await.result(Future.sequence(Vector(
//    Future(factorial(3)),
//    Future(factorial(3))
//  )), 5.seconds)

  Vector("Message").tell

  41.pure[Logged].map(_ + 1)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  val (log, res) = factorial(5).run

  val Vector((logA, ansA), (logB, ansB)) =
    Await.result(Future.sequence(Vector(
      Future(factorial(3).run),
      Future(factorial(5).run)
    )), 5.seconds)

  println("logA: " + logA)
  println("ansA: " + ansA)
  println("logB: " + logB)
  println("ansB: " + ansB)
}
