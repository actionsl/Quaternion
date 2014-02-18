package org.actionsl.algebra

import org.junit.Assert._
import org.junit._

import Quaternion.numberToQuaternion
import Quaternion.rotateVector

class QuaternionTest {

  import Quaternion._

  private val q = Quaternion(1, 0, 0, 0)
  private val qI = Quaternion(0, 1, 0, 0)
  private val qJ = Quaternion(0, 0, 1, 0)
  private val qK = Quaternion(0, 0, 0, 1)

  private val qTip = Quaternion(2, 3, 5, 7)

  @Test
  def identitiesTest() {
    assertTrue((qI * qI) == (-ONE))
    assertTrue((qJ * qJ) == (-ONE))
    assertTrue((qK * qK) == (-ONE))
  }

  @Test
  def productsTest() {
    assertTrue((qI * qJ) == (qK))
    assertTrue((qJ * qI) == (-qK))
    assertTrue((qJ * qK) == (qI))
    assertTrue((qK * qJ) == (-qI))
    assertTrue((qK * qI) == (qJ))
    assertTrue((qI * qK) == (-qJ))

    // conjugate test
    assertTrue((qTip + qI * qTip * qI + qJ * qTip * qJ + qK * qTip * qK) / (-2.0) == qTip.conjugate)
  }

  @Test
  def methodsTest() {

    // multiply by scalar at the back
    assertTrue((qJ * 3) == (Quaternion(0, 0, 3, 0)))

    // versor
    assertTrue(((qTip.versor * qTip.versor).norm - 1.0) < EPS)
    assertTrue((qTip.versor.norm - 1.0) < EPS)

    // inverse
    assertTrue((qTip.inverse) * qTip == ONE)
    assertTrue(qTip * (qTip.inverse) == ONE)
  }

  @Test //http://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
  def rotationsTest() {
    
    val theta = 2 * math.Pi / 3.0
    val axis = Tuple3(1.0, 1.0, 1.0)
    val vector = Tuple3(3.0, 4.0, 5.0)
    
    // rotated vector
    val vPrime = rotateVector(theta, axis, vector)

    assertTrue(vPrime == Tuple3(5.0, 3.0, 4.0))

  }
}