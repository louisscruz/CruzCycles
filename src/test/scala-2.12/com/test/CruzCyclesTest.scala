package com.test

import org.scalatest.FunSpec

import CruzCycles._

class CruzCyclesTest extends FunSpec {
  val three = List("one", "two", "three")
  val four = List("one", "two", "three", "four")

  describe("generateThesaurus") {
    it("should create a map of the correct size") {
      assert(generateThesaurus().size === 121449)
    }
  }

  describe("synonymsFor") {
    describe("when the word does not exist") {
      it("should produce the correct number of synonyms") {
        assert(synonymsFor("piz").size === 0)
      }
    }

    describe("when the word exists") {
      it("should produce the correct number of synonyms") {
        assert(synonymsFor("zip").size === 28)
      }
    }
  }

  describe("antonymsFor") {
    describe("when the word does not exist") {
      it("should return an empty list") {
        assert(antonymsFor("zxcv").size === 0)
      }
    }

    describe("when the word exists") {
      it("should produce the correct number of antonyms") {
        assert(antonymsFor("incomprehensible").size === 2)
      }
    }
  }

  describe("exploreSynonyms") {
    it("should return all the possible synonyms that can be reached") {
      assert(exploreSynonyms("glasses").size == 4)
    }
  }

  describe("nonSymmetricEntries") {
    it("should return all the nonSymmetricEntries") {
      assert(nonSymmetricEntries().size == 8219)
    }
  }

  describe("connectedComponents") {
    it ("shold return all the components") {
      println(connectedComponents().size)
    }
  }

  describe("findCruzCycle") {
    describe("when no cycle exists") {
      describe("when there is no antonym") {
        it("returns an empty list") {
          assert(findCruzCycle("boston cream pie") === List())
        }
      }

      describe("when there happens to be no cycle") {
        it("returns an empty list") {
          val noCycles = List("yellow, brick, road")
          val allInvalid = noCycles forall (el => findCruzCycle(el) === List())
          assert(allInvalid)
        }
      }
    }

    describe("when a cycle exists") {
      it("should return a proper cycle") {
        val cycles = List("all", "some", "generous", "beautiful", "thoughtful")
        val allValid = cycles forall (el => {
          val cycle = findCruzCycle(el)
          antonymsFor(el) contains cycle.last
        })
        assert(allValid)
      }
    }
  }

  describe("minCycle") {
    describe("when there is no min") {
      it("should return an empty list in a list") {
        assert(minCycle(List(List())) === List(List()))
      }
    }

    describe("when there is a min") {
      describe("when there is a single nadir") {
        it("should return the correct cycle") {
          val cycles = List(three, four)
          assert(minCycle(cycles) === List(three))
        }
      }

      describe("when there are multiple nadirs") {
        it("should return the correct cycles") {
          val cycles = List(three, four, three, four)
          assert(minCycle(cycles) === List(three, three))
        }
      }
    }
  }

  describe("maxCycle") {
    describe("when there is no max") {
      it("should return an empty list in a list") {
        assert(maxCycle(List(List())) === List(List()))
      }
    }

    describe("when there is a max") {
      describe("when there is a single apex") {
        it("should return the correct cycle") {
          val cycles = List(three, four)
          assert(maxCycle(cycles) === List(four))
        }
      }

      describe("when there are multiple apexes") {
        it("should return the correct cycles") {
          val cycles = List(three, four, three, four)
          assert(maxCycle(cycles) === List(four, four))
        }
      }
    }
  }
}