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

listOf : g -> Random t g -> Int -> Range t -> [t]
listOf gen elemType n (l, h) =
  case n of
    0 -> []
    n ->
      let rand = randomR <| elemType
          (i, gen') = rand (l,h) gen
      in i::(listOf gen' elemType (n-1) (l, h))