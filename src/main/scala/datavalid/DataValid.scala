package datavalid

import cats.Semigroup
import cats.syntax.either._ // for asLeft and asRight
import cats.syntax.semigroup._ // for |+|
import cats.instances.list._ // for Semigroup

object DataValid extends App {

  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = func(a)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(a)) => e.asLeft
          case (Right(a), Left(e)) => e.asLeft
          case (Right(a1), Right(a2)) => a.asRight
        }

      }
  }

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if(v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if(v < -2) v.asRight
      else List("Must be < -2").asLeft
    }
  val check: CheckF[List[String], Int] = a and b

  println(check(5))
  // res8: Either[List[String],Int] = Left(List(Must be < -2))

  println(check(0))
  // res9: Either[List[String],Int] = Left(List(Must be > 2, Must be < -2))


}
