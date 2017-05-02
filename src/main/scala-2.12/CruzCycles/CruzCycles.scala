package CruzCycles

import scala.io.Source

/**
  * Created by louiscruz on 5/1/17.
  */
class CruzCycles {
  def getFile() = {
    val filename = "thesaurus.txt"
//    Source.fromFile(filename).getLines.map(line => {
//
//    })
//    val thing = for {
//      line <- Source.fromFile(filename).getLines()
//      vals <- line.split(",")
//    } yield vals
//    println(thing)
//    thing
//    val invMap = io.Source.fromFile(filename).getLines.map {
//      l =>
//        val Array(k, _*) = l.split(',')
//        println(l.split(','))
//        k -> Set(k)}.toMap
//    invMap
      val m = Source.fromFile(filename).getLines.map {
        line =>
          println(line)
          line -> line
      }.toMap
      m
  }
}
