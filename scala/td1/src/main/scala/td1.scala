package fr.enst.plnc2014.td1

import scala.collection._
import scala.math._

object TD1
{
  // Placer ici le code de isOdd & friends
  def isOdd(x: Int): Boolean = if (x % 2 == 0) false else true
  def isEven(x: Int): Boolean = !isOdd(x)
  def myWhile(c: => Boolean, f: => Any ) {
    while (c) f
  }

  // The problem of the N queens, which I didn't heard about before this
  // work, is to find how all the possible position of N queens on a chess
  // board game which size is N*N square, with the N queen being no danger
  // for the other one. In the chess game the queen can attack in every
  // direction. So if a queen is at a position (X, Y) she's blocking every
  // position responding to the equations (X, _), (_, Y), (X + N, Y + N) and
  // (X + N, Y - N) (N could be negative)
  // Since we must place N queen on a N*N square and each queen is blocking a
  // row the solution is necessarly one queen by row, we are going to place
  // the queens row by row. Each time a place is available for a queen for
  // place it and we call again the function that will this time check the
  // position available for the next row.

  // Function to determinate if the asked position is a available postion for a
  // queen.
  def positionFree(posCol: Int, posRow: Int,
                   queens: List[(Int, Int)]): Boolean = {
    // For each queen
    for ((qCol: Int, qRow: Int) <- queens) {
      if (posCol == qCol) // Same colone as a queen
        return false
      else if (posRow == qRow) {// Same row as a queen /!\ Should not happend
        return false
        println("This situation should not happend");
      } else if (abs(posCol - qCol) == abs(posRow - qRow)) {
        // On a diagonal of the queen
        return false
      }
    }
    true
  }

  // Function to do a row. For each row every place available for a queen
  // create a leaf where the we check if the this position leads
  def doRow(rowNb: Int, numberOfQueens: Int, queens: List[(Int, Int)],
            f: List[(Int, Int)] => Unit): Unit = {
    for (col: Int <- 1 to numberOfQueens) {
      if (positionFree(col, rowNb, queens)) {
          val newQueens = queens:+ (col, rowNb)
          if (rowNb == numberOfQueens) {
            // We have placed the number of queens recommanded.
            f (newQueens.toList)
          } else  doRow(rowNb + 1, numberOfQueens, newQueens, f)
      }
    }
  }

  def solveQueens(numberOfQueens: Int, f: List[(Int, Int)] => Unit): Unit =
    doRow(1, numberOfQueens, List[(Int, Int)](), f)
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

  def reciprocal: Complex = Complex(real, -im)

  def +(c: Complex): Complex = Complex(real + c.real, im + c.im)
  def -(c: Complex): Complex = Complex(real - c.real, im - c.im)
  def *(c: Complex): Complex = Complex((real * c.real) - (im * c.im),
    (real * c.im + c.real * im))
  def abs: Double = sqrt(real*real + im*im)
  def /(c: Complex): Complex = Complex(((real * c.real) + (im * c.im))/ (c.abs * c.abs),
    ((im * c.real) - (real * c.im))/(c.abs * c.abs))
  def exp: Complex = Complex((math.exp(real) * cos(im)), (math.exp(real) * sin(im)))
}

object Complex {
  // All double are Complex with a imaginary part null.
  implicit def toComplex(d: Double): Complex = Complex(d, 0)
}

object Main extends App
{

  import TD1._

  // Placer ici le code à exécuter
}
