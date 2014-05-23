package fr.enst.plnc2014.td1

import scala.collection._

object TD1
{
  // Placer ici le code de isOdd & friends
  def isOdd(x: Int): Boolean = if (x % 2 == 0) false else true
  def isEven(x: Int): Boolean = !isOdd(x)
}

class ExtSeq[+T](s : Seq[T])
{
  // Here we must apply a global or between f of all the element of the Seq
  def any(f: T => Boolean): Boolean = s.exists(f)
  def all(f: T => Boolean): Boolean = s.count(f) == s.length
}

object ExtSeq
{
  implicit def toExtSeq[T](s: Seq[T]): ExtSeq[T] = new ExtSeq(s)
}

object Main extends App
{

  import TD1._

  // Placer ici le code à exécuter
}