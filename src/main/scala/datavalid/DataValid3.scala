package datavalid

import cats.Semigroup
import cats.data.Validated
import cats.syntax.semigroup._ // for |+|
import cats.syntax.apply._ // for mapN
import cats.data.Validated._ // for Valid and Invalid
import cats.instances.list._ // for Semigroup


object DataValid3 extends App {

  sealed trait Predicate[E, A] {
    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a1) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a2) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  final case class And[E, A](
                              left: Predicate[E, A],
                              right: Predicate[E, A]) extends Predicate[E, A]
  final case class Or[E, A](
                             left: Predicate[E, A],
                             right: Predicate[E, A]) extends Predicate[E, A]
  final case class Pure[E, A](
                               func: A => Validated[E, A]) extends Predicate[E, A]

  val a: Predicate[List[String], Int] =
    Pure { v =>
      if (v > 2) Validated.Valid(v)
      else Validated.Invalid(List("Must be > 2"))
    }

  val b: Predicate[List[String], Int] =
    Pure { v =>
      if (v < -2) Validated.Valid(v)
      else Validated.Invalid(List("Must be < -2"))
    }

  val c: Predicate[List[String], Int] =
    Pure { v =>
      if (v < 2) Validated.Valid(v)
      else Validated.Invalid(List("Must be < 2"))
    }

  val d: Predicate[List[String], Int] =
    Pure { v =>
      if (v > -2) Validated.Valid(v)
      else Validated.Invalid(List("Must be > -2"))
    }

  val predicate: Predicate[List[String], Int] = a and b
  println(predicate(0))


}
