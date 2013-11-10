module RandomGen(RandomGen, 
                 stdGen,
                 next,
                 split,
                 range,
                 mkStdGen,
                 maxInt,
                 minInt,
                 Random,
                 randomR,
                 random,
                 randomInt,
                 randomFloat) where

{-| Library for pure pseudo-random number generation.

This library provides a way to create pseudo-random numbers without the use of
Signals. In addition to this, it provides a way to produce repeatable results
by specifying a seed.

The is almost a direct translation of Haskell's [System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html) module which is an implemenation of the Portable Combined Generator of L'Ecuyer for 32-bit computers. It has a period of roughly 2.30584e18.

-}

type Next g = g -> (Int, g)
type Split g = g -> (g, g)
type Range g = g -> (Int,Int)

{-| RandomGen provides a common interface for number generators. To create one, you must specify three components: @docs next, split, range -}

data RandomGen g
  = RandomGen (Next g) (Split g) (Range g)

{-| The next operation returns an Int that is uniformly distributed in the range returned by genRange (including both end points), and a new generator. -}
next : RandomGen g -> (g -> (Int, g))
next (RandomGen n _ _) = n

{-| The split operation allows one to obtain two distinct random number gnerators. This is very useful in functional programs (For example, when passing a random number generator down to recursive calls), but very little work has been done on statistically robust implementations of split. -}
split : RandomGen g -> Split g
split (RandomGen _ s _) = s

{-| The range operation yields the range of values returned by the generator. -}
range : RandomGen g -> Range g
range (RandomGen _ _ r) = r


data StdGen = StdGen Int Int

{-| Provides an instance of RandomGen that has a range of at least 30 bits.
    
the result of repeatedly using next should be at least as statistically robust as the Minimal Standard Random Number Generator. Until more is known about implementations of split, all we require is that split deliver generators that are (a) not identical and (b) independently robust in the sense just given. -}
stdGen : RandomGen StdGen
stdGen = RandomGen stdNext stdSplit stdRange

{-| The function mkStdGen provides a way of producing an initial generator by mapping an Int into a generator. Distinct arguments should be likely to produce distinct generators. -}
mkStdGen : Int -> StdGen
mkStdGen s = 
  if s < 0 
    then mkStdGen (-s)
    else 
      let q = s `div` (magicNum6-1)
          s1 = s `mod` (magicNum6-1)
          s2 = q `mod` (magicNum7-1)
      in StdGen (s1+1) (s2+1)                         

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

stdNext : StdGen -> (Int, StdGen)
stdNext (StdGen s1 s2) = 
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
    in  (z', StdGen s1'' s2'')

stdSplit : Split StdGen
stdSplit std =
    case std of
      StdGen s1 s2 -> 
        let new_s1 = if s1 == magicNum6-1 then 1 else s1 + 1
            new_s2 = if s2 == 1 then magicNum7-1 else s2 - 1
            std' = snd (stdNext std)
        in case std' of
          StdGen t1 t2 ->
            let left = StdGen new_s1 t2
                right = StdGen t1 new_s2
            in (left, right)

stdRange : Range StdGen
stdRange _ = (0, magicNum8)

type Rand a g = g -> (a, g)
type RandR a g = (a, a) -> g -> (a, g)         

{-| The Random type provides an interface for supplying random values over a range. -}
data Random a g =
  Random (RandR a g) (Rand a g)
         
{-| Given a Generator and a range [low, high] produces a value uniformly distributed between the closed interval and a new generator. -}
randomR : Random a g -> RandR a g
randomR (Random r _) = r

{-| This is the same as randomR but uses a default range -}
random : Random a g -> Rand a g
random (Random _ r)  = r  

{-| Provides an instance of Random for Int values given a RandomGen. The default range is [minInt, maxInt] -}
randomInt : RandomGen g -> Random Int g
randomInt gen =  
  let randr (a,b) rng = randomIval gen (a, b) rng
      rand rng = randr (minInt, maxInt) rng
  in Random randr rand

{-| Provides an instance of Random for Float values given a RandomGen. The default range is (0, 1) -}
randomFloat : RandomGen g -> Random Float g
randomFloat gen =
  let randr (a, b) rng = randomFval gen (a, b) rng
      rand rng = randr (0, 1) rng
  in Random randr rand

randomIval : RandomGen g -> (Int, Int) -> g -> (Int, g)
randomIval gen (l,h) rng =
  if l > h 
    then randomIval gen (h, l) rng
    else 
      let k = h - l + 1
          b = magicNum8 - 1
          n = iLogBase b k
          f n acc g =

            case n of
              0 -> (acc, g)
              n' -> 
                let (x, g') = (next gen) g
                in f (n' - 1) (x + acc * b) g'
          (v, rng') = f n 1 rng
      in (l + v `mod` k, rng')
             
         
iLogBase : Int -> Int -> Int       
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)

randomFval : RandomGen g -> (Float, Float) -> g -> (Float, g)
randomFval gen (l, h) rng = 
  if l > h 
    then randomFval gen (h, l) rng 
    else 
      let (x, rng') = randomIval gen (minInt, maxInt) rng
          scaled = ((l+h)/2) + ((h-l) / (toFloat (maxInt - minInt))) * (toFloat x)
      in (scaled, rng')
             