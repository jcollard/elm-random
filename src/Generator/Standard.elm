module Generator.Standard (standard,
                           int,
                           intRange,
                           float,
                           floatRange,
                           listOf,
                           minInt,
                           maxInt) where

{-|

Generator.Standard is an implemenation of the Generator interface that uses
an implementation of the Portable Combined Generator of L'Ecuyer for 32-bit
computers.

# The Standard Generator
@docs standard

# Generating Functions
@docs int, float, intRange, float, listOf

-}

import Generator as G


type StandardGenerator = G.Generator Standard
type Random a = StandardGenerator -> (a, StandardGenerator)

{-| Given a seed value, creates a standard Generator.
Using the same seed will yield repeatable results.

This generator is almost a direct translation from Haskell's
[System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
module which is an implemenation of the Portable Combined Generator of
L'Ecuyer for 32-bit computers. It has a period of roughly 2.30584e18.-}
standard : Int -> StandardGenerator
standard seed = G.Generator (mkStdGen seed) stdNext stdSplit stdRange

{-| Generate a 32-bit integer in range [minInt32,maxInt32] inclusive. -}
int : Random Int
int = G.int32

{-| Generate an integer in a given range. For example, the expression
`intRange (0,1) generator` will produce either a zero or a one. Note: the
randomness is only enough for 32-bit values. Although this function 
will continue to produce values outside of the range [minInt32, maxInt32],
sufficient randomness is not guaranteed.
-}
intRange : (Int, Int) -> Random Int
intRange = G.int32Range

{-| Generate a float between 0 and 1 inclusive. -}
float : Random Float
float = G.float

{-| Generate a float in a given range. -}
floatRange : (Float, Float) -> Random Float
floatRange = G.floatRange

{-| Generate a list of random values using a generator function.

      -- list of 10 floats in range (0,1):
      listOf float 10 generator

      -- list of 42 integers in range [0,3]
      listOf (intRange (0,3)) 42 generator
-}
listOf : Random a -> Int -> Random [a]
listOf = G.listOf

{-| The maximum value for randomly generated for ints -}
minInt : Int
minInt = G.minInt32

{-| The minimum value for randomly generated for ints -}
maxInt : Int
maxInt = G.maxInt32

data Standard = Standard Int Int

{-| Produce the initial generator state. Distinct arguments should be likely
to produce distinct generator states.
-}
mkStdGen : Int -> Standard
mkStdGen s' =
    let s = max s' -s'
        q  = s `div` (magicNum6-1)
        s1 = s `mod` (magicNum6-1)
        s2 = q `mod` (magicNum7-1)
    in  Standard (s1+1) (s2+1)                         


magicNum0 = 40014
magicNum1 = 53668
magicNum2 = 12211
magicNum3 = 52774
magicNum4 = 40692
magicNum5 = 3791
magicNum6 = 2147483563
magicNum7 = 2137383399
magicNum8 = 2147483562

stdNext : Standard -> (Int, Standard)
stdNext (Standard s1 s2) = 
    -- Div always rounds down and so random numbers are biased
    -- ideally we would use division that rounds towards zero so
    -- that in the negative case it rounds up and in the positive case
    -- it rounds down. Thus half the time it rounds up and half the time it
    -- rounds down
    let k = s1 `div` magicNum1 
        s1' = magicNum0 * (s1 - k * magicNum1) - k * magicNum2
        s1'' = if s1' < 0 then s1' + magicNum6 else s1' 
        k' = s2 `div` magicNum3 
        s2' = magicNum4 * (s2 - k' * magicNum3) - k' * magicNum5
        s2'' = if s2' < 0 then s2' + magicNum7 else s2'
        z = s1'' - s2''
        z' = if z < 1 then z + magicNum8 else z
    in  (z', Standard s1'' s2'')

stdSplit : Standard -> (Standard, Standard)
stdSplit (Standard s1 s2 as std) =
    let new_s1 = if s1 == magicNum6-1 then 1 else s1 + 1
        new_s2 = if s2 == 1 then magicNum7-1 else s2 - 1
        (Standard t1 t2) = snd (stdNext std)
    in  (Standard new_s1 t2, Standard t1 new_s2)

stdRange : Standard -> (Int,Int)
stdRange _ = (0, magicNum8)

