package de.choffmeister.secpwd.security

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import de.choffmeister.secpwd.security.RandomGenerator._

@RunWith(classOf[JUnitRunner])
class RandomGeneratorSpec extends Specification {
  "generate random octets" in {
    val rnd1 = new Array[Byte](24)
    val rnd2 = new Array[Byte](24)
    generateRandomOctets(rnd1)
    generateRandomOctets(rnd2)
    val rnd3 = generateRandomOctets(24)
    val rnd4 = generateRandomOctets(24)

    rnd1 !== rnd2
    rnd1 !== rnd3
    rnd1 !== rnd4
    rnd2 !== rnd3
    rnd2 !== rnd4
    rnd3 !== rnd4
  }

  "generate random integers" in {
    val rnd1 = (1 to 10).map(i => generateRandomInt(25)).toList
    val rnd2 = (1 to 10).map(i => generateRandomInt(25)).toList
    val rnd3 = generateRandomInts(25, 10)

    rnd1.forall(_ >= 0) === true
    rnd1.forall(_ < 25) === true
    rnd2.forall(_ >= 0) === true
    rnd2.forall(_ < 25) === true
    rnd3.forall(_ >= 0) === true
    rnd3.forall(_ < 25) === true

    rnd1 !== rnd2
    rnd1 !== rnd3
    rnd2 !== rnd3
  }
}
