package foldtrav

import scala.language.higherKinds
import cats.syntax.apply._ // for mapN
import cats.Applicative
import cats.instances.future._ // for Applicative
import cats.syntax.applicative._ // for pure

object CatsTraversable extends App {

  def listTraverse[F[_]: Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  import cats.instances.vector._ // for Applicative
  println(listSequence(List(Vector(1, 2), Vector(3, 4))))

 println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))



}
