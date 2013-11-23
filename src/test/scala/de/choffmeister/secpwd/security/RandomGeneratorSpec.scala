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
}