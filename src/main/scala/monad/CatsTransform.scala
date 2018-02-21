package monad

import scala.concurrent.Future
import cats.data.{EitherT, OptionT}
import cats.instances.future._ // for Monad
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.syntax.applicative._ // for pure
import cats.instances.either._ // for Monad
import cats.instances.option._ // for Monad
import cats.data.Writer
import cats.syntax.writer._
import cats.syntax.flatMap._ // for flatMap
import scala.language.higherKinds

object CatsTransform  extends App {

//  // Alias Either to a type constructor with one parameter:
//  type ErrorOr[A] = Either[String, A]
//  // Build our final monad stack using OptionT:
//  type ErrorOrOption[A] = OptionT[ErrorOr, A]
//
//  val a = 10.pure[ErrorOrOption]
//  // a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
//  val b = 32.pure[ErrorOrOption]
//  // b: ErrorOrOption[Int] = OptionT(Right(Some(32)))
//
//  type FutureEither[A] = EitherT[Future, String, A]
//  type FutureEitherOption[A] = OptionT[FutureEither, A]
//
//  val futureEitherOr: FutureEitherOption[Int] =
//    for {
//      a <- 10.pure[FutureEitherOption]
//      b <- 32.pure[FutureEitherOption]
//    } yield a + b
//
//  println(futureEitherOr)
//
//  val c = a.flatMap(x => b.map(y => x + y))
//  // c: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(42)))
//
//  // Create using apply:
//  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
//  // errorStack1: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(10)))
//
//  import cats.data.Writer
//  type Logged[A] = Writer[List[String], A]
//  // Methods generally return untransformed stacks:
//  def parseNumber(str: String): Logged[Option[Int]] =
//    util.Try(str.toInt).toOption match {
//      case Some(num) => Writer(List(s"Read $str"), Some(num))
//      case None => Writer(List(s"Failed on $str"), None)
//    }
//  // Consumers use monad transformers locally to simplify composition:
//  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
//    import cats.data.OptionT
//    val result = for {
//      a <- OptionT(parseNumber(a))
//      b <- OptionT(parseNumber(b))
//      c <- OptionT(parseNumber(c))
//    } yield a + b + c
//    result.value
//  }
//  // This approach doesn't force OptionT on other users' code:
//  val result1 = addAll("1", "2", "3")
//  // result1: Logged[Option[Int]] = WriterT((List(Read 1, Read 2, Read 3),Some(6)))
//  val result2 = addAll("1", "a", "3")
  // result2: Logged[Option[Int]] = WriterT((List(Read 1, Failed on a), None))

}
