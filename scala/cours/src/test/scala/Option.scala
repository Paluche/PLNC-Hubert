package fr.enst.plnc

// Int, CHar, String, Double. Tout les types classiques de Java.
// List, Seq (elements qui se suivent), Array (taille fixe)
// Map

//  Parenthesis are not mandatory if not agurment
abstract class Option[+T]
{
  def isDefinded: Boolean
  def get: T
  def bind[U](f: T => Option[U]): Option[U]
}

// val non modifiable: good
// var modifiable: bad
// In the definition xe have define the constructor and the function get.
// case redefined the equality, and toString
case class Some[+T](val get: T)  extends Option[T]
{
  val isDefined(): Boolean = true
  // No more useful with case  override def toString = "Some(" + get + ")"
  //override val toString = s"Some($get)"
  def bind[U](f: T => Option[U]) = f(get)
}

// Compagnion d'object
object Some {
  def apply[T](v: T) = new Some(v)
}

case object None extends Option[Nothing]
{
  val isDefined = false
  def get = sys.error("no get for None")
  def bind[U](f: T => Option[U]) = None
}

class A
class B extends A

abstract class List[+T]
{
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
  def ::[U >: T](start: U): List[U] = Cons(start, this)

  override lazy val toString =
    if (isEmpty "()" else s"(${internatToString})"

  def internatToString: String =
    if (isEmpty) ""
    else if (tail.isEmpty) s"$head"
    else s"$head, ${tail.internatToString}"
}

case object Nil extends List[Nothing]
{
  def head = sys.error("no head")
  def tail = sys.error("no tail")
}

case class Cons[+T] (head: T, tail: List[T]) extends List[T]
{
  def head = sys.error("no head")
  def tail = sys.error("no tail")
}
