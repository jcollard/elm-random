module RandomGen(Generator,
                 standard,
                 int,
                 intRange,
                 float,
                 floatRange,
                 listOf,
                 minInt,
                 maxInt) where

{-| Library for pure pseudo-random number generation. A basic usage would look like this:

```
-- Create a random number generator. This must be threaded through the program.
generator = standard 42

-- Create a random float.
(randomFloat, generator') = float generator

-- Create a random integer in a range.
(randomInt, generator'') = intRange (0,10) generator'
```

The explicit use of a generator makes it possible to reproduce results
by using the same seed. In the example above the seed was 42, and every time
that code runs it will give the same values for `randomFloat` and `randomInt`.

# Generating Numbers
@docs int, float, intRange, floatRange, listOf

# Generators
@docs standard, Generator

-}

{-| Generator provides a common interface for number generators.
To create one, you must specify three components: next, split, range

 * The `state` field holds the current state of the generator.
 * The `next` operation returns an Int that is uniformly distributed in the
   range returned by genRange (including both end points), and a new generator.
 * The `split` operation allows one to obtain two distinct random number
   generators. This is very useful in functional programs (For example, when
   passing a random number generator down to recursive calls), but very
   little work has been done on statistically robust implementations of split.
 * The `range` operation yields the range of values returned by the generator.
-}
type Generator state = {
  state : state,
  next  : state -> (Int, state),
  split : state -> (state, state),
  range : state -> (Int,Int)
}

data Standard = Standard Int Int

{-| Given a seed value, creates a standard Generator.
Using the same seed will yield repeatable results.

This generator is almost a direct translation from Haskell's
[System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
module which is an implemenation of the Portable Combined Generator of
L'Ecuyer for 32-bit computers. It has a period of roughly 2.30584e18.-}
standard : Int -> Generator Standard
standard seed = Generator (mkStdGen seed) stdNext stdSplit stdRange

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

{-| The maximum value for randomly generated ints -}
maxInt : Int
maxInt = 2147483647

{-| The minimum value for randomly generated ints -}
minInt : Int
minInt = -2147483648

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

{-| Generate an integer in range [minInt,maxInt] inclusive. -}
int : Generator g -> (Int, Generator g)
int = intRange (minInt, maxInt)

{-| Generate an integer in a given range. For example, the expression
`intRange (0,1) generator` will produce either a zero or a one.
-}
intRange : (Int, Int) -> Generator g -> (Int, Generator g)
intRange (lo, hi) generator =
  if lo > hi
  then intRange (hi, lo) generator
  else 
    let k = hi - lo + 1
        b = magicNum8 - 1
        n = iLogBase b k
        f n acc state =
            case n of
              0 -> (acc, state)
              _ -> let (x, state') = generator.next state
                   in  f (n - 1) (x + acc * b) state'
        (v, state') = f n 1 generator.state
    in  (lo + v `mod` k, { generator | state <- state' })

iLogBase : Int -> Int -> Int       
iLogBase b i =
    if i < b then 1 else 1 + iLogBase b (i `div` b)

{-| Generate a float between 0 and 1 inclusive. -}
float : Generator g -> (Float, Generator g)
float = floatRange (0,1)

{-| Generate a float in a given range. -}
floatRange : (Float, Float) -> Generator g -> (Float, Generator g)
floatRange (lo, hi) generator = 
    if lo > hi
    then floatRange (hi, lo) generator
    else 
      let (x, generator') = intRange (minInt, maxInt) generator
          scaled = (lo+hi)/2 + ((hi-lo) / toFloat (maxInt - minInt)) * toFloat x
      in (scaled, generator')

{-| Create a list of random values using a generator function.

      -- list of 10 floats in range (0,1):
      listOf float 10 generator

      -- list of 42 integers in range [0,3]
      listOf (intRange (0,3)) 42 generator
-}
listOf : (Generator g -> (number, Generator g)) ->
         Int -> Generator g -> ([number], Generator g)
listOf = listOfHelp []

listOfHelp : [number] -> (Generator g -> (number, Generator g)) ->
             Int -> Generator g -> ([number], Generator g)
listOfHelp list generate n generator =
    if n < 1
    then (reverse list, generator)
    else
        let (value, generator') = generate generator
        in  listOfHelp (value :: list) generate (n-1) generator'