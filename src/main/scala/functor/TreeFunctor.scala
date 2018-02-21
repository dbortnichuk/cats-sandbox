package functor

import cats.Functor
import sandbox.models.Models.{Box, Branch, Leaf, Tree}

import scala.concurrent.{ExecutionContext, Future}

object TreeFunctor {



  implicit class FunctorOps[F[_], A](src: F[A]) {
    def map[B](func: A => B)(implicit functor: Functor[F]): F[B] = functor.map(src)(func)
  }

  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A, B](value: Option[A])(func: A => B): Option[B] = value.map(func)
    }

  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] = value.map(func)
    }

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
        tree match {
          case Branch(left, right) =>
            Branch(map(left)(func), map(right)(func))
          case Leaf(value) =>
            Leaf(func(value))
        }
    }

  trait Codec[A] {
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      val self = this
      new Codec[B] {
        def encode(value: B): String =
          self.encode(enc(value))
        def decode(value: String): B =
          dec(self.decode(value))
      }
    }
  }
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
  implicit val doubleCodec: Codec[Double] = stringCodec.imap[Double](_.toDouble, _.toString)
  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap[Box[A]](Box(_), _.value)


  def main(args: Array[String]): Unit = {

    //println(Branch(Leaf(10), Leaf(20)).map(_ * 2))

    println(Tree.leaf(100).map(_ * 2))
    // res10: wrapper.Tree[Int] = Leaf(200)
    println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))
    // res11: wrapper.Tree[Int] = Branch(Leaf(20),Leaf(40))
    println(encode(123.4))
    // res0: String = 123.4
    println(decode[Double]("123.4"))
    // res1: Double = 123.4
    println(encode(Box(123.4)))
    // res2: String = 123.4
    println(decode[Box[Double]]("123.4"))
    // res3: Box[Double] = Box(123.4)
  }

}
