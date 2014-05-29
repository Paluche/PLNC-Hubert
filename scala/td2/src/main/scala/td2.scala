package fr.plnc.td2

import scala.io._
import rx.lang.scala._

object TD2 {
  def fromURL(url: String): Observable[Char] =
    Observable.defer(Observable.from(scala.io.Source.fromURL(url).toIterable, schedulers.IOScheduler.apply))

  def getContent(url: String): Observable[String] =
    Observable.defer( Observable[String] {
    (subscriber: Subscriber[String]) =>
      var observableFromURL: Observable[Char] = fromURL(url)
      var string: String = ""

      observableFromURL.subscribe(
        { (c: Char) => string = string :+ c },
        { (t: Throwable) => println(s"Exception: $t") },
        { () =>  subscriber.onNext(string)})
    })
}



object Main extends App
{
  import TD2._

  println("Start, instanciate fromURL(Google)")
  //val google: Observable[Char] = fromURL("http://www.rfc1149.net/blog/")
  //google.subscribe(
  //  { n => println(n) },
  //  { t: Throwable => println(s"Exception: $t") },
  //  { () => println("Observable is terminated") })

  val googleStr: Observable[String] = getContent("http://www.rfc1149.net/blog/")
  googleStr.subscribe(
    { n => println(n) },
    { t: Throwable => println(s"Exception: $t") },
    { () => println("Observable is terminated") })

}

