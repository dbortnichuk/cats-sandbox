package monad

import cats.syntax.either._ // for map and flatMap

object CatsEither extends App {

  val a = 3.asRight[String]
  // a: Either[String,Int] = Right(3)
  val b = 4.asRight[String]
  // b: Either[String,Int] = Right(4)
  for {
    x <- a
    y <- b
  } yield x*x + y*y
  // res4: scala.util.Either[String,Int] = Right(25)
  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchNonFatal(sys.error("Badness")))
  println(Either.fromTry(scala.util.Try("foo".toInt)))
  println(Either.fromOption[String, Int](None, "Badness"))
  println("Error".asLeft[Int].getOrElse(0))
  println("Error".asLeft[Int].orElse(2.asRight[String]))
  println((-1).asRight[String].ensure("Must be non-negative!")(_ > 0))

  println{
    "error".asLeft[Int].recover {
      case str: String => -1
    }
  }

}
