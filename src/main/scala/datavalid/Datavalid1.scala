package datavalid

import cats.Semigroup
import cats.syntax.either._ // for asLeft and asRight
import cats.syntax.semigroup._ // for |+|
import cats.instances.list._ // for Semigroup

object Datavalid1 extends App {

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) =>
          (left(a), right(a)) match {
            case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
            case (Left(e), Right(a)) => e.asLeft
            case (Right(a), Left(e)) => e.asLeft
            case (Right(a1), Right(a2)) => a.asRight
          }
      }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]

  val a: Check[List[String], Int] =
    Pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val b: Check[List[String], Int] =
    Pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check: Check[List[String], Int] = a and b

  println(check(5))
  // res8: Either[List[String],Int] = Left(List(Must be < -2))
  println(check(0))
  // res9: Either[List[String],Int] = Left(List(Must be > 2, Must be < -2))
}
