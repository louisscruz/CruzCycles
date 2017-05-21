import scala.io.Source

package object CruzCycles {
  type SynonymAntonymTuple = (Set[String], Set[String])
  type Thesaurus = Map[String, SynonymAntonymTuple]
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
        val nextWords = for {
          word <- current
          nextWords <- ((synonymsFor(word) -- visited) -- current)
        } yield nextWords

        exploreSynonymsAcc(nextWords, visited ++ nextWords)
      }
    }

    exploreSynonymsAcc(Set(word), Set(word))
  }

  lazy val transposedThesaurus: Thesaurus = {
    thesaurus.foldLeft(Map[String, (Set[String], Set[String])]()) { case (acc, (word, (synonyms, _))) =>
      synonyms.foldLeft(acc) {
        case (acc, synonym) => acc + (synonym -> (((acc.getOrElse(synonym, (Set[String](), Set[String]()))._1 + word), Set[String]())))
      }
    }
  }

  lazy val thesaurusList = thesaurus.map { case (k, v) => (k, v) } toList

  def nonSymmetricSynonymEntries(): Set[(String, String)] = {
    def nonSymmetricSynonymEntriesAcc(mappings: List[(String, SynonymAntonymTuple)], entries: Set[(String, String)]): Set[(String, String)] = mappings match {
      case Nil => entries
      case (word, (synonyms, _)) :: xs => {
        val nonSymmetric = synonyms filter {
          synonym => !(synonymsFor(synonym) contains word)
        } map { el => (word, el) }

        if (nonSymmetric.size > 0) nonSymmetricSynonymEntriesAcc(xs, entries ++ nonSymmetric) else nonSymmetricSynonymEntriesAcc(xs, entries)
      }
    }

    nonSymmetricSynonymEntriesAcc(thesaurusList, Set())
  }

  def nonSymmetricAntonymEntries(): Set[(String, String)] = {
    def nonSymmetricAntonymEntriesAcc(mappings: List[(String, SynonymAntonymTuple)], entries: Set[(String, String)]): Set[(String, String)] = mappings match {
      case Nil => entries
      case (word, (_, antonyms)) :: xs => {
        val nonSymmetric = (antonyms filter { antonym => !(antonymsFor(antonym) contains word) }) map { el => (word, el) }

        if (nonSymmetric.size > 0) nonSymmetricAntonymEntriesAcc(xs, entries ++ nonSymmetric) else nonSymmetricAntonymEntriesAcc(xs, entries)
      }
    }

    nonSymmetricAntonymEntriesAcc(thesaurusList, Set())
  }

//  def exploreSynonymsDepthFirst(siblings: Set[String]): List[String] = {
//    def exploreDepthFirstAcc(siblings: Set[String], acc: Set[String]): Set[String] = {
//
//    }
//
//    exploreDepthFirstAcc(siblings, Set())
//  }
//
//  def connectedComponents(): List[Set[String]] = {
//    def firstPass(): List[String] = {
//      def firstPassAcc(acc: List[String]) = {
//
//      }
//
//      firstPassAcc(List())
//    }
//
//    def secondPass(stack: List[String]): List[Set[String]] = {
//      def secondPassAcc(stack: List[String], acc: List[Set[String]], visited: Set[String]): List[Set[String]] = stack match {
//        case Nil => acc
//        case x :: xs => visited.contains(x) match {
//          case true => secondPassAcc(xs, acc, visited)
//          case false => {
//
//            val explored = exploreSynonymsDepthFirst(Set(x))
//            secondPassAcc(xs, explored.toSet :: acc, visited ++ explored)
//          }
//        }
//      }
//
//      secondPassAcc(stack, List(), Set())
//    }
//
//    secondPass(firstPass())
//  }

//  def maxConnectedComponent(): Set[String] = connectedComponents().reduceLeft((a, b) => if (a.size > b.size) a else b)

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
