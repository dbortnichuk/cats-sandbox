package monoid

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
trait Monoid1[A] extends Semigroup[A] {
  def empty: A
}
object Monoid1 {
  def apply[A](implicit monoid: Monoid1[A]) =
    monoid
}

object Monoids {

  implicit val booleanAndMonoid: Monoid1[Boolean] =
    new Monoid1[Boolean] {
      def combine(a: Boolean, b: Boolean) = a && b
      def empty = true
    }

  implicit val booleanOrMonoid: Monoid1[Boolean] =
    new Monoid1[Boolean] {
      def combine(a: Boolean, b: Boolean) = a || b
      def empty = true
    }

  implicit val booleanEitherMonoid: Monoid1[Boolean] =
    new Monoid1[Boolean] {
      def combine(a: Boolean, b: Boolean) =
        (a && !b) || (!a && b)
      def empty = false
    }

  implicit val booleanXnorMonoid: Monoid1[Boolean] =
    new Monoid1[Boolean] {
      def combine(a: Boolean, b: Boolean) =
        (!a || b) && (a || !b)
      def empty = true
    }

  implicit def setUnionMonoid[A]: Monoid1[Set[A]] =
    new Monoid1[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b
      def empty = Set.empty[A]
    }

  implicit val intMonoid: Monoid1[Int] = new Monoid1[Int] {
    def combine(a: Int, b: Int) = a + b
    def empty = 0
  }

  val intSetMonoid = Monoid1[Set[Int]]

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]) =
        a intersect b
    }

//  implicit def symDiffMonoid[A]: Monoid[Set[A]] =
//    new Monoid[Set[A]] {
//      def combine(a: Set[A], b: Set[A]): Set[A] =
//        (a diff b) union (b diff a)
//      def empty: Set[A] = Set.empty
//    }



}
