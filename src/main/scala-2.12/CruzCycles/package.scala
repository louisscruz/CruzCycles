import scala.io.Source

package object CruzCycles {
  type Thesaurus = Map[String, (Set[String], Set[String])]

  lazy val thesaurus = generateThesaurus()

  def generateThesaurus(): Thesaurus = {
    def getThesaurusLines(): List[String] = {
      val filename = "Thesaurus.txt"
      Source.fromFile(filename).getLines.toList
    }

    def generateThesaurusAcc(thesaurus: Thesaurus, lines: List[String]): Thesaurus = lines match {
      case Nil => thesaurus.withDefaultValue((Set[String](), Set[String]()))
      case x :: xs => {
        val split = x.split(';')
        val word = split(0)
        val synonyms = split(1).split(',').toSet
        val antonyms = if (split.length == 2) Set[String]() else split(2).split(',').toSet

        val newThesaurus: Thesaurus = thesaurus updated (word, (synonyms, antonyms))
        generateThesaurusAcc(newThesaurus, xs)
      }
    }

    generateThesaurusAcc(Map(), getThesaurusLines())
  }

  def synonymsFor(word: String): Set[String] = thesaurus(word)._1

  def antonymsFor(word: String): Set[String] = thesaurus(word)._2

  def findCruzCycle(source: String): List[String] = {
    val thesaurus = generateThesaurus()
    val (synonyms, antonyms) = thesaurus(source)

    if (antonyms.size == 0) return List()

    def findCruzCyclesAcc(target: String, visited: Set[String], currentWords: Set[String]): List[String] = {
      synonymsFor(target).toList
    }

    findCruzCyclesAcc(source, Set[String](), synonymsFor(source))
  }
}
