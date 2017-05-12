package com.test

import org.scalatest.FunSpec

import CruzCycles._

class CruzCyclesTest extends FunSpec {
  val thesaurus = generateThesaurus()

  describe("generateThesaurus") {
    it("should create a map of the correct size") {
      assert(thesaurus.size === 121449)
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
//        println(antonymsFor("big"))
//        thesaurus foreach (el => println(el._2._2.size))
        assert(antonymsFor("incomprehensible").size === 2)
      }
    }
  }

  describe("findCruzCycle") {
    describe("when no cycle exists") {
      describe("when there is no antonym") {
        it("returns an empty list") {
          assert(findCruzCycle("boston cream pie") === List())
        }
      }
    }

    describe("when a cycle exists") {
      it("should return a proper cycle") {
//        assert(findCruzCycle("big") === List("big"))
        println(findCruzCycle("big"))
      }
    }
  }
}