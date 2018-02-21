package monad


import cats.Monad

import scala.annotation.tailrec
import sandbox.models.Models.{Branch, Leaf, Tree}
import sandbox.models.Models.Tree._
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap

object CatcsCustom extends App {



  val optionMonad = new Monad[Option] {

    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] = opt flatMap fn

    def pure[A](opt: A): Option[A] = Some(opt)

    @tailrec
    def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }

  implicit val treeMonad = new Monad[Tree] {

    def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] = {
      tree match {
        case Leaf(x) => func(x)
        case Branch(l, r) => Branch(flatMap(l)(func), flatMap(r)(func))
      }
    }

    def pure[A](tree: A): Tree[A] = Leaf(tree)

    def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Tree[B]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            l match {
              case Branch(_, _) =>
                loop(l :: r :: next, closed)
              case Leaf(Left(value)) =>
                loop(func(value) :: r :: next, closed)
              case Leaf(Right(value)) =>
                loop(r :: next, pure(value) :: closed)
            }
          case Leaf(Left(value)) :: next =>
            loop(func(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            closed match {
              case head :: tail =>
                loop(next, Branch(head, pure(value)) :: tail)
              case Nil =>
                loop(next, pure(value) :: closed)
            }
          case Nil =>
            closed
        }

      loop(List(func(arg)), Nil).head
    }
  }

  val t1 = branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
  // res3: wrapper.Tree[Int] = Branch(Branch(Leaf(99),Leaf(101)),Branch(Leaf(199),Leaf(201)))
  println(t1)


  val t2 = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

  println(t2)

}
