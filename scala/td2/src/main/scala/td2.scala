package fr.plnc.td2

import scala.io._
import rx.lang.scala._
import spray.json._
import DefaultJsonProtocol._

object TraktAPIVal {
  // User ID
  val traktAPIKey: String = ??? // Enter here your API key

  // API url requests
  val traktAPIURL: String = "http://api.trakt.tv/"

  // Time request
  val traktAPIServerTime: String = traktAPIURL + "server/time.json/" + traktAPIKey
}

object TD2 {
  import TraktAPIVal._

  def fromURL(url: String): Observable[Char] =
    Observable.defer(Observable.from(scala.io.Source.fromURL(url).toIterable,
      schedulers.IOScheduler.apply))

  // By the way, I don't know why this function must ise the fromURL function.
  // The function scala.io.Source.fromURL returns a BufferedSource that dispose
  // of a function to directly returns the content in String format.
  def getContent(url: String): Observable[String] =
    Observable.defer(Observable[String] {
    (subscriber: Subscriber[String]) =>
      var string: String = ""

      fromURL(url).subscribe(
        { (c: Char) => string = string :+ c },
        { (t: Throwable) => println(s"Exception: $t") },
        { () =>  subscriber.onNext(string)})
    })

  def getJSON(url: String): Observable[JsValue] =
    Observable.defer(Observable[JsValue] {
      (subscriber: Subscriber[JsValue]) =>
        getContent(url).subscribe(
        { (c: String) => subscriber.onNext(c.parseJson)},
        { (t: Throwable) => println(s"Exception: $t") },
        { () => println("Observable is terminated") })
    })

  def traktTimestamp: Observable[Int] = (
    Observable.defer(Observable[Int] {
      (subscriber: Subscriber[Int]) =>
        getJSON(traktAPIServerTime).subscribe(
          { (j: JsValue) =>
          subscriber.onNext(j.asJsObject.getFields("timestamp").head.convertTo[Int]) })
      })
  )

  def getTraktJSON(path: String, args: Iterable[String] = List()): Observable[JsValue] =
  getJSON(traktAPIURL + path + ".json/" + traktAPIKey + "/" + args.mkString("/"))
}

object Main extends App
{
  import TD2._
  traktTimestamp.subscribe({(t: Int) => println(s"Time: ${t}")})

  getTraktJSON("show/season", List("the-walking-dead","1")).subscribe(
    {x => println(x.prettyPrint)},
    { (t: Throwable) => println(s"Exception: $t") },
    { () => println("Observable is terminated") })
}

