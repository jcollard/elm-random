elm-random
==========

Library for pure pseudo-random number generation.

This library provides a way to create pseudo-random numbers without the use of
Signals. In addition to this, it provides a way to produce repeatable results
by specifying a seed.

The is almost a direct translation of Haskell's [System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html) module which is an implemenation of the Portable Combined Generator of L'Ecuyer for 32-bit computers. It has a period of roughly 2.30584e18.


