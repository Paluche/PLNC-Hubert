package fr.plnc.td2

import scala.io._
import rx.lang.scala._
import rx.lang.scala.schedulers

object TD2 {
  def fromURL(url: String): Observable[Char] = Observable[Char] {
    (subscriber: Subscriber[Char]) =>
      println(s"requesting ${url}")
      val buffer: BufferedSource = scala.io.Source.fromURL(url)

      while(buffer.hasNext && !subscriber.isUnsubscribed)
        subscriber.onNext(buffer.next)

      subscriber.onCompleted
    }
}



object Main extends App
{
  import TD2._

  println("Start, instanciate fromURL(Google)")
  val google: Observable[Char] = fromURL("http://www.google.fr")

  google.subscribe(
    { n => println(n) },
    { t: Throwable => println(s"Exception: $t") },
    { () => println("Observable is terminated") })
}

