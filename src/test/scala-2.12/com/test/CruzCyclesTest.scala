package com.test

import org.scalatest.FunSpec

import CruzCycles._

/**
  * Created by louiscruz on 5/1/17.
  */
class CruzCyclesTest extends FunSpec {
  describe("generateThesaurus") {
    val thesaurus = generateThesaurus()

    it("should create a map of the correct size") {
      assert(thesaurus.size === 121449)
    }
  }

  describe("synonymsFor") {
    it("should produce the correct number of synonyms") {
      assert(synonymsFor("zip").size === 28)
    }
  }

  describe("findCruzCycles") {
    it("should do something") {
      println(findCruzCycles("test"))
    }
  }
}