package CruzCycles

import scala.io.Source

/**
  * Created by louiscruz on 5/1/17.
  */
class CruzCycles {
  def getFile() = {
    val filename = "Thesaurus.txt"
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

//    def reduceList(list: List[List[String]]): List[List[String]] = list match {
//      case x :: xs => {
//        if (xs.isEmpty) List(x)
//        else if (x.head == xs.head.head) reduceList(xs)
//        else {
//          println(x)
//          x :: reduceList(xs)
//        }
//      }
//    }

    def reduceList(list: List[List[String]]): List[List[String]] = {
      def reduceAcc(list: List[List[String]], acc: List[List[String]]): List[List[String]] = {
        if (list.tail.isEmpty) list.head :: acc
        else reduceAcc(list.tail, acc ++ List(list.head))
      }
      reduceAcc(list, List())
    }
    val lines = Source.fromFile(filename).getLines.toList.map(el => el.split(',').toList)
//    val lines = Source.fromFile(filename).getLines.toList
//    map {
//      line => line.split(',').toList
//    }.toList.groupBy(el => el.head)
//    val lines = Source.fromFile(filename).getLines.toList
//    println(reduceList(lines))
    println(reduceList(lines))
  }
}
