package functor


import cats.Show
import cats.functor.Contravariant
import cats.instances.string._
import cats.syntax.contravariant._
import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.invariant._ // for imap
import cats.syntax.semigroup._ // for |+|

object ContraInveCats extends App {

  val showString = Show[String]
  val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")
  println(showSymbol.show('dave))
  // res2: String = 'dave
  println(showString.contramap[Symbol](_.name).show('dave))

//  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)
//  Monoid[Symbol].empty
//  // res5: Symbol = '
//  'a |+| 'few |+| 'words
//  // res6: Symbol = 'afewwords

  
}
