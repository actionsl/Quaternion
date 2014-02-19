Quaternion
==========

Scala implementation of Quaternion algebra


Quaternions were described by William Hamilton (of Hamiltonian fame) in the 19th century as a faster algebra
for rotating vectors in 3D space.

In programming there are many implementations of quaternions on the web but most don't provide an explicit
function for performing rotations.

In this implementation the Quaternion companion object has a method called rotateVector(...) that rotates a
vector by an angle around an axis where the vectors are described by Tuple3 objects chosen for their simplicity.

The code looks similar to other examples on the internet but this is a computational example of convergent
evolution. The purpose of this code is to provide a quaternion rotation example written in scala.
