package crdt

import cats.Monoid
import cats.syntax.semigroup._ // for |+|

trait KeyValueStore[F[_,_]] {

  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {

  implicit val mapInstance: KeyValueStore[Map] =
    new KeyValueStore[Map] {

      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

      def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

      override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
        f.getOrElse(k, default)

      def values[K, V](f: Map[K, V]): List[V] = f.values.toList
    }

//  implicit def gcounterInstance[F[_,_], K, V]
//  (implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]) =
//    new GCounter[F, K, V] {
//      def increment(f: F[K, V])(key: K, value: V)
//                   (implicit m: Monoid[V]): F[K, V] = {
//        val total = f.getOrElse(key, m.empty) |+| value
//        f.put(key, total)
//      }
//      def merge(f1: F[K, V], f2: F[K, V])
//               (implicit b: BoundedSemiLattice[V]): F[K, V] =
//        f1 |+| f2
//      def total(f: F[K, V])(implicit m: Monoid[V]): V =
//        f.values.combineAll
//    }

  implicit class KvsOps[F[_,_], K, V](f: F[K, V]) {

    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }


}
