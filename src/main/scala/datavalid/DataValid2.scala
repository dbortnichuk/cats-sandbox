package datavalid

import cats.Semigroup
import cats.data.Validated
import cats.syntax.semigroup._ // for |+|
import cats.syntax.apply._ // for mapN
import cats.data.Validated._ // for Valid and Invalid
import cats.instances.list._ // for Semigroup

object DataValid2 extends App{

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)
    def or(that: Check[E, A]): Check[E, A] = Or(this, that)
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  val a: Check[List[String], Int] =
    Pure { v =>
      if (v > 2) Validated.Valid(v)
      else Validated.Invalid(List("Must be > 2"))
    }

  val b: Check[List[String], Int] =
    Pure { v =>
      if (v < -2) Validated.Valid(v)
      else Validated.Invalid(List("Must be < -2"))
    }

  val c: Check[List[String], Int] =
    Pure { v =>
      if (v < 2) Validated.Valid(v)
      else Validated.Invalid(List("Must be < 2"))
    }

  val d: Check[List[String], Int] =
    Pure { v =>
      if (v > -2) Validated.Valid(v)
      else Validated.Invalid(List("Must be > -2"))
    }

  val check: Check[List[String], Int] = a and b
  val check1: Check[List[String], Int] = c and d
  val check2: Check[List[String], Int] = a or b

  println(check(5))
  // res8: Either[List[String],Int] = Left(List(Must be < -2))
  println(check(0))

  println(check1(0))
  println(check1(3))

  println(check2(0))
  println(check2(3))

}
