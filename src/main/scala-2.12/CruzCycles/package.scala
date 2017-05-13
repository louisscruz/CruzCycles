import scala.io.Source
import scala.util.Random

package object CruzCycles {
  type Thesaurus = Map[String, (Set[String], Set[String])]
  type StringMapping = Map[String, String]

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
    val (synonyms, antonyms) = thesaurus(source)
    if (antonyms.size == 0) return List()

    def findCruzCyclesAcc(currentWords: Set[String], visited: StringMapping): List[String] = {
      def backTrack(antonym: String): List[String] = {
        def backTrackAcc(word: String, acc: List[String]): List[String] =
          if (word == source) word :: acc else backTrackAcc(visited(word), word :: acc)

        backTrackAcc(antonym, List())
      }

      def generateNextWords(): StringMapping = {
        def generateNextWordsAcc(siblings: List[String], acc: StringMapping): StringMapping = siblings match {
          case List() => acc
          case x :: xs => {
            val newSynonyms = (for {
              synonym <- synonymsFor(x)
              if (!(acc contains synonym) && !(visited contains synonym) && !(currentWords contains synonym))
            } yield (synonym, x)).toMap
            generateNextWordsAcc(xs, acc ++ newSynonyms)
          }
        }

        generateNextWordsAcc(currentWords.toList, Map())
      }

      for (word <- currentWords) if (antonyms contains word) return backTrack(word)
      val nextWords = generateNextWords()
      if (nextWords.size < 1)
        List()
      else
        findCruzCyclesAcc(nextWords.keySet, visited ++ nextWords)
    }
    val initialMap = (for (synonym <- synonyms) yield (synonym, source)).toMap
    findCruzCyclesAcc(synonyms, initialMap)
  }
}
