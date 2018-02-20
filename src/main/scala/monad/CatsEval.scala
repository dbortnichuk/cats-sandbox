package monad

import cats.Eval

object CatsEval extends  App{

//  def factorial(n: BigInt): BigInt = if(n == 1) n else n * factorial(n - 1)
//  factorial(50000)

  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }
  //println(factorial(50000).value)

  def foldRightNS[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = //not stack safe
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) => b.map(fn(a, _))}.value


  println(foldRight((1 to 1000000).toList, 0L)(_ + _))

}
