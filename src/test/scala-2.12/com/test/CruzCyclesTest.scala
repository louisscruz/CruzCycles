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
      describe("when the inversion boolean is not given") {
        it("should produce the correct number of synonyms") {
          assert(synonymsFor("piz").size === 0)
        }
      }

      describe("when the inversion boolean is set to false") {
        it("should produce the correct number of synonyms") {
          assert(synonymsFor("piz", false).size === 0)
        }
      }

      describe("when the inversion boolean is set to true") {
        it("should produce the correct number of synonyms") {
          assert(synonymsFor("piz", true).size === 0)
        }
      }
    }

    describe("when the word exists") {
      describe("when the inversion boolean is not given") {
        it("should produce the correct number of synonyms") {
          assert(synonymsFor("zip").size === 28)
        }
      }

      describe("when the inversion boolean is set to false") {
        it("should produce the correct number of synonyms") {
          assert(synonymsFor("zip", false).size === 28)
        }
      }

      describe("when the inversion boolean is set to true") {
        it("should produce the correct number of synonyms") {
          assert(synonymsFor("zip", true).size === 23)
        }
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
    describe("when the inversion boolean is not given") {
      it("should return all the possible synonyms that can be reached") {
        assert(exploreSynonyms("glasses").size == 4)
      }
    }

    describe("when the inversion boolean is set to false") {
      it("should return all the possible synonyms that can be reached") {
        assert(exploreSynonyms("glasses", false).size == 4)
      }
    }

    describe("when the inversion boolean is set to true") {
      it("should return all the possible synonyms that can be reached") {
        assert(exploreSynonyms("glasses", true).size == 4)
      }
    }
  }

  describe("transposedThesaurus") {
    it("should return the correct edge relationships") {
      val answer = thesaurus forall (el => el._2._1 forall (a => transposedThesaurus contains a))
      assert(answer)
    }
  }

  describe("nonSymmetricSynonymEntries") {
    it("should return all the non-symmetric synonym entries") {
      val answer = nonSymmetricSynonymEntries() forall { el => !(synonymsFor(el._2) contains el._1) }
      assert(answer)
    }
  }

  describe("nonSymmetricAntonymEntries") {
    it("should return all the non-symmetric antonym entries") {
      val answer = nonSymmetricAntonymEntries() forall { el => !(antonymsFor(el._2) contains el._1) }
      assert(answer)
    }
  }

  describe("exploreSynonymsDepthFirst") {
    describe("when the inversion boolean is not given") {
      it("should return all stuff") {
        assert(exploreSynonymsDepthFirst("glasses")._1 == List("glasses", "spectacles", "specs", "eyeglasses"))
      }
    }

    describe("when the inversion boolean is set to false") {
      it("should return all stuff") {
        assert(exploreSynonymsDepthFirst("glasses", false)._1 == List("glasses", "spectacles", "specs", "eyeglasses"))
      }
    }

    describe("when the inversion boolean is set to true") {
      it("should return all stuff") {
        assert(exploreSynonymsDepthFirst("canarese", true)._1 == List("glasses", "eyeglasses", "spectacles", "specs"))
      }
    }
  }

  describe("connectedComponents") {
    // In need of a quick way to properly test this
    it ("should return all the components") {
      val components = connectedComponents()
      assert(components.size < thesaurus.size)
    }
  }

  describe("maxConnectedComponent") {
    it("should return the largest") {
      val component = maxConnectedComponent()
      println(component.size)
      assert(connectedComponents() forall (el => el.size <= component.size))
    }

    it("should do something") {
      println(meanConnectedComponentSize())
//      val c = connectedComponents().sortWith(_.size > _.size)
//      c foreach (el => println(el.size))
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