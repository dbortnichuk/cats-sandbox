package sandbox.printable


import sandbox.models.Models.{Box, Cat}

trait Printable[A] {
  self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }
}

object PrintableInstances {
  implicit val printableString: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val printableInt: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }

  //  implicit def boxPrintable[A](implicit p: Printable[A]) =
  //    new Printable[Box[A]] {
  //      def format(box: Box[A]): String = p.format(box.value)
  //    }

  implicit def boxPrintable[A](implicit p: Printable[A]) = p.contramap[Box[A]](_.value)

  implicit val printableCat: Printable[Cat] =
    new Printable[Cat] {
      def format(cat: Cat): String = {
        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val color = Printable.format(cat.color)
        s"$name is a $age year-old $color cat."
      }
    }
}

object Printable {
  def format[A: Printable](value: A): String = implicitly[Printable[A]].format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))

}

object PrintableSyntax {

  implicit class PrintableOps[A: Printable](value: A) {
    def format: String = implicitly[Printable[A]].format(value)

    def print(implicit p: Printable[A]): Unit = println(p.format(value))

  }

}

//class Tst2() extends Tst {
//
//  def test(t: Tst) = t.x
//
//}
//
//class Tst1() extends Tst {
//
//  def test(t: Tst) = t.x
//
//}

class Tst(a: Int, b: Int) {
  def this(c: Int) {
    this(c, 0)
  }

  def this() {
    this(0)
  }

  private val x = 0

  def other(o: Tst) = println(o.x)

}

object Tst {
  def apply(a: Int, b: Int): Tst = {
    val t = new Tst(a, b)
    println(t.x)
    t.other(new Tst())
    t
  }

}