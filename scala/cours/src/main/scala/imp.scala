package fr.enst.plnc

import scala.language.implicitConversions

class EnrichedString(s: String) {
  def double: String = s + s
}

object Imp extends App{
  //implicit def convertToEnrichedString(s: String) = new EnrichedString(s)

  implicit class convertToEnrichedString(s: String) {
    def double = s + s
  }

  println("foobar")

//  def createTwoButton(label1: String, label2: String) (implicit ctx: Context) {
//  ...
//  }
//  createTwoButton("b1", "b2")(mycontext)
//  {
//    implicit mycontext: Context = SomeContext()
//    createTwoButton("b1", "b2")
//  }
  def callDouble(s: String)(f:String => Unit) = {
      f(s)
  }
  callDouble("xyz")(println(_))

  def f(x:Int) = x+1
  def g(x:Int) = x
  println((1 to 10).reduce(_ + _))
  println((1 to 10).reduce((a: Int, b:Int) => f(g(a)) + b))

  println((1 to 10).collect {
    case x if x % 2 == 0 => x * 10
  })

  callDouble("k,ddkjnfvkjfdn") {
    case s if s.size == 5 => println(s)
    case _                => println("Not the right size")
  }

  trait Function[+P, -R] {
    def apply(p: P): R
  }

  class A
  class B extends A

  class C
  class D extends C
  val b: Function[B, C] = (_.size)
  val a: Function[A, D] = b

}

// Map is a ditionnry Map(("first", "hubert"), ("last", "Lefeve"))
