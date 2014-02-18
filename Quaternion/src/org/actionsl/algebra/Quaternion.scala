package org.actionsl.algebra


/*
 * Implementation of the Quaternion algebra including a simple method for rotating vectors
 * 
 * @param Tuple4 - holds the quaternion components. This data type was chosen to enforce a more 
 * structured data input. Conversion are simple and there are implicit conversions in the companion.
 * 
 */
case class Quaternion(val t4: Tuple4[Double, Double, Double, Double]) {

  import math._
  import Quaternion._

  def this(q: Quaternion) = this(q.re, q.i, q.j, q.k)
  def this(re: Double = 0.0, v: Tuple3[Double,Double,Double]) = this(re, v._1, v._2, v._3)

  // use traditional (i,j,k) notation for the imaginary part of the Quaternion
  private val re = t4._1
  private val i = t4._2
  private val j = t4._3
  private val k = t4._4

  // get the distinct parts of the Quaternion
  lazy val  qRe = re
  lazy val  qVector = Tuple3(i, j, k)

  lazy val conjugate = Quaternion(re, -i, -j, -k)

  //  ||p|| - the mathematical form
  lazy val norm = sqrt(abs(re * re + i * i + j * j + k * k))

  // unit quaternion Uq called the versor of q: q/norm  
  lazy val  versor = this / norm

  // Quaternion inverse - q*q^(-1) = q^(-1)*q = (1,0,0,0)
  lazy val  inverse = conjugate / (norm * norm)

  // operators
  def +(b: Quaternion): Quaternion = Quaternion(re + b.re, i + b.i, j + b.j, k + b.k)
  def -(b: Quaternion): Quaternion = Quaternion(re - b.re, i - b.i, j - b.j, k - b.k) // used for norms ||p-q||

  // a * b is not commutative (the operator* is not commutative to be precise)
  def *(b: Quaternion): Quaternion = Quaternion(
    re * b.re - i * b.i - j * b.j - k * b.k,
    re * b.i + i * b.re + j * b.k - k * b.j,
    re * b.j - i * b.k + j * b.re + k * b.i,
    re * b.k + i * b.j - j * b.i + k * b.re)

    // post multiply by double - use implicit conversion for pre-multiply
  def *(r: Double): Quaternion = Quaternion(r * re, r * i, r * j, r * k)
  def /(r: Double): Quaternion = Quaternion(re / r, i / r, j / r, k / r) 

  def unary_- = Quaternion(-re, -i, -j, -k)
  def unary_~ = conjugate	// not often used

  // test for component equality uses EPS limit
  def ==(b: Quaternion): Boolean =
    abs(re - b.re) < EPS && abs(i - b.i) < EPS && abs(j - b.j) < EPS && abs(k - b.k) < EPS

  override def toString = "(%8.4f, %8.4f, %8.4f, %8.4f)".format(re, i, j, k)
}

object Quaternion {
  
  import math._
  
  type QVector = Tuple3[Double, Double, Double]	

  // used to test equality for real elements a == b if abs(a-b)< EPS - keep it close
  private[algebra] val EPS = 1.0E-10
  
  // convenience constants
  val ZERO = Quaternion(0.0, 0.0, 0.0, 0.0)
  val ONE = Quaternion(1.0, 0.0, 0.0, 0.0)
  
  // use Number view not implicit Numeric[T] because you only need the conversions
  implicit def numberToQuaternion[T <% Number](arg: T): Quaternion =  Quaternion(arg.doubleValue(),0,0,0)

  // convert a real 3d vector to the imaginary part of the Quaternion
  implicit def tuple3ToQuaternion(v: Tuple3[Double, Double, Double]): Quaternion = Quaternion(0, v._1, v._2, v._3)
  
  /*
   * @param theta	angle of rotation 
   * @param	axis	Tuple3[Double] axis of rotation
   * @param	vector	Tuple3[Double] vector to be rotated
   * @return 	Tuple3[Double] resulting rotated vector
   */  
  def rotateVector(theta: Double, axis: QVector, vector: QVector): QVector = {
    require(axis.norm>0.0)
    val r = qRotator(theta, axis)
    (r * vector * r.inverse).qVector	// calls implicit tuple3ToQuaternion
  }

  // Quaternion(cos(theta/2), (sin(theta/2)/Axis.length)*Axis) - make use of implicits
  // 4 constructor calls - slower but clear
  private def qRotator(theta: Double, axis: QVector): Quaternion = 
    cos(theta / 2.0) + new Quaternion(0.0, axis)*(sin(theta / 2.0) / axis.norm)	// calls numberToQuaternion
 }
