package fr.enst.plnc

trait Shout {
  def name: String
  // Without default value
  //def shout: String
  def shout: String = s"${name}"
}

trait Fury extends Shout {
  override def shout = s"comfortable ${super.shout}"
}


// class XXXX extends <primaru class> with <1st trait> <2nd trait>
class ClassRoom (val name: String)

trait Classy extends Animal {
  def price: Int = 300
}

class Animal (val name: String) extends Shout {
  override def shout = "Roar"
}

class Mammal (name: String) extends Animal(name) with Classy

class Human (val name: String) extends Shout {
  override def shout = "Pute"
}

//object MainProgram extends App {
//
//  val a = new Animal("Raoul")
//  val b = new Human("Gerard")
//
//  val cl = new ClassRoom("B312") with Shout
//
//  println(a.shout)
//}
