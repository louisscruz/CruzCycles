package com.test

import org.scalatest.FlatSpec

import CruzCycles._

/**
  * Created by louiscruz on 5/1/17.
  */
class CruzCyclesTest extends FlatSpec {
  "Something" should "do something" in {
    val CruzCycles = new CruzCycles()
    println(CruzCycles.getFile())
    assert(1 === 1)
  }
}

