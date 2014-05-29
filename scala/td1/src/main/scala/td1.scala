package fr.enst.plnc2014.td1

import scala.collection._

object TD1
{
  // Placer ici le code de isOdd & friends
  def isOdd(x: Int): Boolean = if (x % 2 == 0) false else true
  def isEven(x: Int): Boolean = !isOdd(x)
  def myWhile(c: => Boolean, f: => Any ) {
    while (c) f
  }
}

class ExtSeq[+T](s : Seq[T]) {
  // Here we must apply a global or between f of all the element of the Seq
  def any(f: T => Boolean): Boolean = s.exists(f)
  def all(f: T => Boolean): Boolean = s.count(f) == s.length
}

object ExtSeq {
  implicit def toExtSeq[T](s: Seq[T]): ExtSeq[T] = new ExtSeq(s)
}


class ExtCond(c: => Boolean ) {
  def doWhile(f: => Any) {
    while (c) f
  }
}

object ExtCond {
  implicit def toExtCond(c: => Boolean): ExtCond = new ExtCond(c)
}


case class Complex(real: Double, im: Double) {
  override def toString =
      if ((real == 0) && (im == 0))
          "0.0"
      else if (real == 0)
        im.toString + "i"
      else if (im == 0)
        real.toString
      else if (im > 0)
        real.toString + "+" + im.toString + "i"
      else
        real.toString + im.toString + "i"
}


object Main extends App
{

  import TD1._

  // Placer ici le code à exécuter
}
