elm-random
==========

Library for pure pseudo-random number generation.

The Generator Library provides an interface for generating pure pseudo-random
sequences specified by a seed. This allows for repeatable sequences.

To get started immediately, check out the Generator.Standard module which is an 
implemenation of this interface that provides an implementation of the Portable 
Combined Generator of L'Ecuyer for 32-bit computers. This implementation 
provides enough randomness for most purposes.

# Generator.Standard API

[The Standard Generator](#the-standard-generator)

[Generating Functions](#generating-functions)

* [Random a](#random-a)
* [int](#int)
* [intRange](#intrange)
* [float](#float)
* [floatRange](#floatrange)
* [listOf](#listof)

[Example Usage](#example-usage)

Generator.Standard is an implemenation of the Generator interface that uses
an implementation of the Portable Combined Generator of L'Ecuyer for 32-bit
computers.

## The Standard Generator

```haskell
type StandardGenerator = Generator.Generator Standard
standard : Int -> StandardGenerator
```

Given a seed value, creates a standard Generator.
Using the same seed will yield repeatable results.

This generator is almost a direct translation from Haskell's
[System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
module which is an implemenation of the Portable Combined Generator of
L'Ecuyer for 32-bit computers. It has a period of roughly 2.30584e18.


## Generating Functions

#### Random a
```haskell
type Random a = StandardGenerator -> (a, StandardGenerator)
```

The Random type is a function that takes in a StandardGenerator and produces
a pair containing a Random value and a StandardGenerator that should be
propogated to future generations to produce the next element in the sequence.


#### int
```haskell
int : Random Int
```
Generate a 32-bit integer in range [minInt,maxInt] inclusive.

#### intRange
```haskell
intRange : (Int, Int) -> Random Int
```
Generate an integer in a given range. For example, the expression
`intRange (0,1) generator` will produce either a zero or a one. Note: the
randomness is only enough for values within the range [minInt, maxInt].
 Although this function will continue to produce values outside of the 
range [minInt, maxInt], sufficient randomness is not guaranteed.

#### minInt
```haskell
minInt : Int
```
The maximum value for randomly generated for ints

#### maxInt
```haskell
maxInt : Int
```
The minimum value for randomly generated for ints

#### float
```haskell
float : Random Float
```

Generate a float between 0 and 1 inclusive.

#### floatRange
```haskell
floatRange : (Float, Float) -> Random Float
```
Generate a float in a given range.

#### listOf
```haskell
listOf : Random a -> Int -> Random [a]
```
Generate a list of random values using a generator function.

```haskell
--list of 10 floats in range (0,1):
listOf float 10 generator
```

```haskell
-- list of 42 integers in range [0,3]
listOf (intRange (0,3)) 42 generator
```


## Example Usage

```haskell
module Example where

-- Generator.Standard provides a concrete implementation of Generator that
-- has enough randomness for most purposes.
import open Generator.Standard

-- Generates a float between 0 and 1 using the standard generator seeded with 0.
-- Notice that the generating function returns a pair containing the generated
-- element along with the resulting nextGenerator. The nextGenerator can
-- be used to generate more elements in the sequence by suppling it
-- in place of (standard n).
generateFloat = 
  let (val, nextGenerator) = float (standard 0)
  in val

-- Generates a list of 10 ints ranging from 0 to 100 using the standard
-- generator seeded with 0.
-- Notice that listOf returns a pair containing the list of the
-- specified size along with the resulting nextGenerator. The 
-- nextGenerator could then be used to produce more elements in
-- the sequence at another time by supplying it in place of (standard n)
generateIntList = 
  let generatingFunction = intRange (0, 100)
      (vals, nextGenerator) = listOf generatingFunction 10 (standard 0)
  in vals
     
-- Generates a list of 15 ints where the first 5 elements are in the range [0, 9]
-- the middle 5 elements are in the range [10, 19] and the last 5 elements
-- are in th range [20,29] using the standard generator seeded with 0.
-- Notice that the generation function listOf returns a pair containing
-- a list and the generator to produce the next part of the sequence. This
-- is then propogated to the next generator function.
generateMany =
  let initialGenerator = standard 0
      (first, generator') = listOf (intRange (0, 9)) 5 initialGenerator
      (second, generator'') = listOf (intRange (10, 19)) 5 generator'
      (third, _) = listOf (intRange (20, 29)) 5 generator''
  in first ++ second ++ third

main = flow down [asText generateFloat, asText generateIntList, asText generateMany]
```
