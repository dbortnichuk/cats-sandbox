package validated


import cats.Semigroup
import cats.data.Validated
import cats.instances.list._
import cats.syntax.either._
import sandbox.models.Models.User // for catchOnly
import cats.instances.list._ // for Semigroupal
import cats.syntax.apply._ // for mapN

object CatsValidated extends  App {

  type AllErrorsOr[A] = Validated[List[String], A]

//  Semigroupal[AllErrorsOr].product(
//    Validated.invalid(List("Error 1")),
//    Validated.invalid(List("Error 2"))
//  )

  val v = Validated.valid[List[String], Int](123)
  // v: cats.data.Validated[List[String],Int] = Valid(123)
  val i = Validated.invalid[List[String], Int](List("Badness"))
  // i: cats.data.Validated[List[String],Int] = Invalid(List(Badness))


  import cats.syntax.validated._ // for valid and invalid
  123.valid[List[String]]
  // res2: cats.data.Validated[List[String],Int] = Valid(123)
  List("Badness").invalid[Int]
  // res3: cats.

  123.valid.map(_ * 100)
  // res17: cats.data.Validated[Nothing,Int] = Valid(12300)
  "?".invalid.leftMap(_.toString)
  // res18: cats.data.Validated[String,Nothing] = Invalid(?)
  123.valid[String].bimap(_ + "!", _ * 100)
  // res19: cats.data.Validated[String,Int] = Valid(12300)
  "?".invalid[Int].bimap(_ + "!", _ * 100)
  // res20: cats.data.Validated[String,Int] = Invalid(?!)


  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] = data.get(name).toRight(List(s"$name field not specified"))

  val getName = getValue("name") _
  // getName: FormData => FailFast[String] = <function1>

  println(getName(Map("name" -> "Dade Murphy")))
  // res29: FailFast[String] = Right(Dade Murphy)

  println(getName(Map()))
  // res30: FailFast[String] = Left(List(name field not specified))

  type NumFmtExn = NumberFormatException

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumFmtExn](data.toInt).leftMap(_ => List(s"$name must be an integer"))

  println(parseInt("age")("11"))
  // res33: FailFast[Int] = Right(11)

  println(parseInt("age")("foo"))
  // res34: FailFast[Int] = Left(List(age must be an integer))

  def nonBlank(name: String)(data: String): FailFast[String] = Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): FailFast[Int] = Right(data).ensure(List(s"$name must be non-negative"))(_ >= 0)

  def readName(data: FormData): FailFast[String] =
    getValue("name")(data).
      flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data).
      flatMap(nonBlank("age")).
      flatMap(parseInt("age")).
      flatMap(nonNegative("age"))

  readName(Map("name" -> "Dade Murphy"))
  // res41: FailFast[String] = Right(Dade Murphy)
  readName(Map("name" -> ""))
  // res42: FailFast[String] = Left(List(name cannot be blank))
  readName(Map())
  // res43: FailFast[String] = Left(List(name field not specified))
  println(readAge(Map("age" -> "11")))
  // res44: FailFast[Int] = Right(11)
  readAge(Map("age" -> "-1"))
  // res45: FailFast[Int] = Left(List(age must be non-negative))
  println(readAge(Map()))
  // res46: FailFast[Int] = Left(List(age field not specified))

  def readUser(data: FormData): FailSlow[User] =
    (readName(data).toValidated, readAge(data).toValidated).mapN(User.apply)

  println(readUser(Map("name" -> "Dave", "age" -> "37")))
  // res48: FailSlow[User] = Valid(User(Dave,37))
  println(readUser(Map("age" -> "-1")))
  // res49: FailSlow[User] = Invalid(List(name field not specified, age must be non-negative))

}
