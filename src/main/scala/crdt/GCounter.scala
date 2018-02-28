package crdt

import cats.Monoid
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.foldable._ // for

trait GCounter[F[_, _], K, V] {

  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: Monoid[V]): V

}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter

  implicit def mapInstance[K, V]: GCounter[Map, K, V] = {
    new GCounter[Map, K, V] {
      def increment(map: Map[K, V])(key: K, value: V)
                   (implicit m: Monoid[V]): Map[K, V] = {
        val total = map.getOrElse(key, m.empty) |+| value
        map + (key -> total)
      }

      def merge(map1: Map[K, V], map2: Map[K, V])
               (implicit b: BoundedSemiLattice[V]): Map[K, V] = map1 |+| map2

      def total(map: Map[K, V])
               (implicit m: Monoid[V]): V = map.values.toList.combineAll

    }
  }

}


//final case class GCounter[A](counters: Map[String,A]) {
//
//  def increment(machine: String, amount: A)(implicit m: Monoid[A]) = {
//    val value = amount |+| counters.getOrElse(machine, m.empty)
//    GCounter(counters + (machine -> value))
//  }
//  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
//    GCounter(this.counters |+| that.counters)
//
//  def total(implicit m: Monoid[A]): A =
//    this.counters.values.toList.combineAll
//}
