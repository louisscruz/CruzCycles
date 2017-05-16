import scala.io.Source

package object CruzCycles {
  type Thesaurus = Map[String, (Set[String], Set[String])]
  type StringMapping = Map[String, String]
  type Cycle = List[String]
  type CycleList = List[Cycle]

  lazy val thesaurus = generateThesaurus()

  def generateThesaurus(): Thesaurus = {
    def getThesaurusLines(): List[String] = {
      val filename = "thesaurus.txt"
      Source.fromFile(filename).getLines.toList
    }

    def generateThesaurusAcc(thesaurus: Thesaurus, lines: List[String]): Thesaurus = lines match {
      case Nil => thesaurus.withDefaultValue((Set(), Set()))
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

  def exploreSynonyms(word: String): Set[String] = {
    def exploreSynonymsAcc(current: Set[String], visited: Set[String]): Set[String] = {
      if (current.size < 1)
        visited
      else {
        val nextWords = (for {
          word <- current
          nextWords <- (synonymsFor(word) -- visited)
        } yield nextWords)
        exploreSynonymsAcc(nextWords, visited ++ nextWords)
      }
    }

    exploreSynonymsAcc(Set(word), Set(word))
  }

  def findCruzCycle(source: String): Cycle = {
    val (synonyms, antonyms) = thesaurus(source)
    if (antonyms.size == 0) return List()

    def findCruzCyclesAcc(currentWords: Set[String], visited: StringMapping): Cycle = {
      if (currentWords.isEmpty) return List()
      def backTrack(antonym: String): Cycle = {
        def backTrackAcc(word: String, acc: Cycle): Cycle =
          if (word == source) word :: acc else backTrackAcc(visited(word), word :: acc)

        backTrackAcc(antonym, List())
      }

      def generateNextWords(): StringMapping = {
        def generateNextWordsAcc(siblings: Cycle, acc: StringMapping): StringMapping = siblings match {
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
      findCruzCyclesAcc(nextWords.keySet, visited ++ nextWords)
    }

    val initialMap = (for (synonym <- synonyms) yield (synonym, source)).toMap
    findCruzCyclesAcc(synonyms, initialMap)
  }

  def allCycles(): CycleList = thesaurus map { case (k, _) => findCruzCycle(k) } toList

  def cycleComparison(cycles: CycleList, temp: Int, cmp: (Int, Int) => Int): CycleList = {
    def cycleComparisonAcc(cycles: CycleList, i: Int, acc: CycleList): CycleList = cycles match {
      case Nil => List(List())
      case x :: Nil => cmp(x.size, i) match {
        case 1 => List(x)
        case 0 => x :: acc
        case -1 => acc
      }
      case x :: xs => cmp(x.size, i) match {
        case 1 => cycleComparisonAcc(xs, x.size, List(x))
        case 0 => cycleComparisonAcc(xs, i, x :: acc)
        case -1 => cycleComparisonAcc(xs, i, acc)
      }
    }

    cycleComparisonAcc(cycles, temp, List(List()))
  }

  def minComparator(length: Int, i: Int): Int = if (length < i) 1 else if (length > i) -1 else 0

  def maxComparator(length: Int, i: Int): Int = if (length > i) 1 else if (length < i) -1 else 0

  def minCycle(cycles: CycleList): CycleList = cycleComparison(cycles, Int.MaxValue, minComparator)

  def maxCycle(cycles: CycleList): CycleList = cycleComparison(cycles, Int.MinValue, maxComparator)
}
