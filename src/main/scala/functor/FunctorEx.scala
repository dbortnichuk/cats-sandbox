package functor


import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._

import scala.language.higherKinds

object FunctorEx {

  def myMethod[F[_]] = {
    // Reference F without underscores:
    //val functor = Functor.apply[F]
    // ...
  }

  // Declare f specifying parameters:
  val f = (x: Int) => x * 2

  val g = (x: Int) => x + 1

  val h = (x: Int) => "hello" + x
  // Reference f without parameters:
  val f2 = f andThen g andThen h

  val f3 = f compose g


  val list1 = List(1, 2, 3)
  // list1: List[Int] = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)
  // list2: List[Int] = List(2, 4, 6)
  val option1 = Option(123)
  // option1: Option[Int] = Some(123)
  val option2 = Functor[Option].map(option1)(_.toString)

  val func = (x: Int) => x + 1
  // func: Int => Int = <function1>
  val liftedFunc = Functor[Option].lift(func)


  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)


  def main(args: Array[String]): Unit = {
    //println( liftedFunc(Option(1)))

    val func =
      ((x: Int) => x.toDouble).
        andThen(x => x + 1).
        andThen(x => x * 2).
        andThen(x => x + "!")

    //println(func(123))

    println(doMath(Option(20)))
    println(doMath(List(1, 2, 3)))

    implicit class FunctorOps[F[_], A](src: F[A]) {
      def map[B](func: A => B)
                (implicit functor: Functor[F]): F[B] =
        functor.map(src)(func)
    }

    implicit val optionFunctor: Functor[Option] =
      new Functor[Option] {
        def map[A, B](value: Option[A])(func: A => B): Option[B] =
          value.map(func)
      }

    import scala.concurrent.{ExecutionContext, Future}
    implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
      new Functor[Future] {
        def map[A, B](value: Future[A])(func: A => B): Future[B] =
          value.map(func)
      }

    //    val box = Box[Int](123)
    //    println(box.map(value => value + 1))

  }


}
