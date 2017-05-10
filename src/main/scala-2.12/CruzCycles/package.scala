/**
  * Created by louisstephancruz on 5/4/17.
  */

import scala.io.Source

package object CruzCycles {
  type Thesaurus = Map[String, (Set[String], Set[String])]

  def generateThesaurus(): Map[String, (Set[String], Set[String])] = {
    def getThesaurusLines(): List[String] = {
      val filename = "Thesaurus.txt"
      Source.fromFile(filename).getLines.toList
    }

    def generateThesaurusAcc(thesaurus: Thesaurus, lines: List[String]): Thesaurus = lines match {
      case Nil => thesaurus
      case x :: xs => {
        val split = x.split(';')
        val word = split(0)
        val synonyms = split(1).split(',').toSet
        val antonyms = if (split.length == 2) Set[String]() else Set(split(2))

        val newThesaurus: Thesaurus = thesaurus updated (word, (synonyms, antonyms))
        generateThesaurusAcc(newThesaurus, xs)
      }
    }

    generateThesaurusAcc(Map(), getThesaurusLines())
  }

  def synonymsFor(word: String): Set[String] = generateThesaurus()(word)._1

  def findCruzCycles(source: String): List[String] = {
    val thesaurus = generateThesaurus()
    thesaurus(source)._1

    def findCruzCyclesAcc(visited: Set[String], originalWord: String, currentWord: String): List[String] = {
      List("test")
    }

    findCruzCyclesAcc(Set[String](), source, source)
  }
}
