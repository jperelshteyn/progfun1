import scala.collection.immutable.::

def flatten(xs: List[Any]): List[Any] =
  if (xs.isEmpty) List()
  else {
    xs.head match {
      case y :: ys => flatten(y :: ys) ++ flatten(xs.tail)
      case x => List(x) ++ flatten(xs.tail)
    }
  }


def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (same, others) = xs1 span (y => y == x)
    (x, same.length + 1) :: encode(others)
}


encode(List("a", "a", "a", "b", "c", "c", "a"))



def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, z) => f(x) :: z )


def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (x, z) =>  1 + z )


val letters = List("a", "a", "a", "b", "c", "c", "a")
def isA(l: String): Boolean = l == "a"

mapFun(letters, isA)
lengthFun(letters)


def isPrime(n: Int): Boolean = (2 until n).forall(x => n % x > 0)

isPrime(11)
isPrime(21)

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queenCoords = queens zip (queens.length - 1 to 0 by -1)
  queenCoords.forall{ case (c, r) => c != col && r != row &&
    math.abs(c - col) != math.abs(r - row)}
}

val ret = isSafe(2, List(3, 1, 1))

class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

//  def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))
//
//  def adjust(term: (Int, Double)): (Int, Double) = {
//    val (exp, coeff) = term
//    exp -> (coeff + terms(exp))
//  }

  def +(other: Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    terms + (term._1 -> (terms.getOrElse(term._1, 0.0) + term._2))
  }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff + "x^" + exp) mkString " + "
}

new Poly(1 -> 2.2, 2 -> 3.3) + new Poly(1 -> 2.2, 2 -> 3.3)
