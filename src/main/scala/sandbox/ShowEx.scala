package sandbox

import cats.{Eq, Show}
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import sandbox.models.Models.Cat
import cats.syntax.eq._

object ShowEx {
  implicit val catShow = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (c1, c2) =>
      c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
    }

}
