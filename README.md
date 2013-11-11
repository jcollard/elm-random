elm-random
==========

Library for pure pseudo-random number generation.

This library provides a way to create pseudo-random numbers without the use of
Signals. In addition to this, it provides a way to produce repeatable results
by specifying a seed.

The is almost a direct translation of Haskell's [System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html) module which is an implemenation of the Portable Combined Generator of L'Ecuyer for 32-bit computers. It has a period of roughly 2.30584e18.

### Random number generators

<a id="randomgen"></a>
The data type [RandomGen](#randomgen) provides a common interface to random number generators.

```haskell
data RandomGen g 
  = RandomGen (Next g) (Split g) (Range g)
```

<a id="next"></a>

```haskell
type Next g = g -> (Int, g)
```

The [Next](#next) operation returns an **Int** that is uniformly distributed in 
the range returned by the **Range** operator, and a new generator.

<a id="split"></a>

```haskell
type Split g = g -> (g, g)
```

The [Split](#split) operation allows one to obtain two distinct random number 
generators. This is very useful in functional programs (For example, 
when passing a random number generator down to recursive calls), but 
very little work has been done on statistically robust implementations of split.

<a id="range"></a>

```haskell
type Range g = g -> (Int, Int)
```

The [Range](#range) operation yields the range of values returned by the generator.

There are selector functions for each of the [RandomGen](#randomgen) functions.

```haskell
next : RandomGen g -> Next g
split : RandomGen g -> Split g
range : RandomGen g -> Range g
```

### The Standard Random Number Generator

This library provides an instance of [RandomGen](#randomgen) that has a range of 
at least 30 bits. The result of repeatedly using [next](#next) should be at least 
as statistically robust as the Minimal Standard Random Number Generator. 
Until more is known about implementations of split, all we require is that 
split deliver generators that are (a) not identical and (b) independently robust 
in the sense just given. 

```haskell
stdGen : RandomGen StdGen
````

<a id="mkstdgen"></a>

```haskell
mkStdGen : Int -> StdGen
```
The function [mkStdGen](#mkstdgen) provides a way of producing an initial generator 
by mapping an **Int** seed into a generator. Distinct arguments should be likely
to produce distinct generators.

### Random values of various types

Given a generator, the **Random** type allows the programmer to extract 
random values of a variety of types.

```haskell
data Random a g =
  Random (RandR a g) (Rand a g)
```

<a id="randr"></a>
```haskell
type RandR a g = (a, a) -> g -> (a, g)
```
The [RandR](#randr) operation takes a range (lo,hi) and a random number generator g,
and returns a random value uniformly distributed in the closed interval [lo, hi],
together with a new generator. It is unspecified what happens if lo>hi. For continuous
types there is no requirement that the values lo and hi are ever produced, but they
may be, depending on the implementaiton and the interval.


<a id="rand"></a>
```haskell
type Rand a g = g -> (a, g)
```
The [Rand](#rand) operation is the same as [RandR](#randr) but uses a default range
determined by the type.

Two selectors for the [RandR](#randr) and [Rand](#rand) types are provided.

```haskell
randomR : Random a g -> RandR a g
random : Random a g -> Rand a g
```

### Implementations of Random Generators
<a id="randomint"></a>
```haskell
randomInt : RandomGen g -> Random Int g
```
Given a [RandomGen](#randomgen) produces a [Random](#random) of type **Int** using the strategy
of the generator provided. The default range of the [Rand](#rand) operation
is [[minInt](#minint),[maxInt](#maxint)].

<a id="minint"></a> <a id="maxint"></a>
```haskell
minInt : Int
minInt = -2147483648

maxInt : Int
maxInt = 2147483647
```

<a id="randomfloat"></a>
```haskell
randomFloat : RandomGen g -> Random Float g
```
Given a [RandomGen](#randomgen) produces a [Random](#random) of type **Float** using the strategy
of the generator provided. The default range of the [Rand](#rand) operation is
[0,1].


### Example Usage
The example below shows how to generate lists of random **Int** and **Float** values.

```haskell
module Example where

import open RandomGen

-- Create a list of 5 floats ranging from 0 to 1 using the seed 0
floats = listOfFloat 0 5 (0, 1)

-- Create a list of 10 ints ranging from 0 to 100 using the seed 0
ints = listOfInt 0 10 (0, 100)

main = flow down [asText floats, asText ints]

type Range a = (a, a)

listOfInt : Int -> Int -> Range Int -> [Int]
listOfInt seed = listOf (mkStdGen seed) (randomInt stdGen)
       
listOfFloat : Int -> Int -> Range Float -> [Float]
listOfFloat seed = listOf (mkStdGen seed) (randomFloat stdGen)

listOf : a -> Random b a -> number -> Range b -> [b]
listOf gen elemType n (l, h) =
  case n of
    0 -> []
    n ->
      let rand = randomR <| elemType
          (i, gen') = rand (l,h) gen
      in i::(listOf gen' elemType (n-1) (l, h))
```
