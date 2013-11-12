module Example where

-- Generator.Standard provides a concrete implementation of Generator that
-- has enough randomness for most purposes.
import open Generator
import open Generator.Standard

-- Generates a float between 0 and 1 using the standard generator seeded with 0.
-- Notice that the generating function returns a pair containing the generated
-- element along with the resulting nextGenerator. The nextGenerator can
-- be used to generate more elements in the sequence by suppling it
-- in place of (standard n).
generateFloat = 
  let (val, nextGenerator) = float (generator 0)
  in val

-- Generates a list of 10 ints ranging from 0 to 100 using the standard
-- generator seeded with 0.
-- Notice that listOf returns a pair containing the list of the
-- specified size along with the resulting nextGenerator. The 
-- nextGenerator could then be used to produce more elements in
-- the sequence at another time by supplying it in place of (standard n)
generateIntList = 
  let generatingFunction = int32Range (0, 100)
      (vals, nextGenerator) = listOf generatingFunction 10 (generator 0)
  in vals
     
-- Generates a list of 15 ints where the first 5 elements are in the range [0, 9]
-- the middle 5 elements are in the range [10, 19] and the last 5 elements
-- are in th range [20,29] using the standard generator seeded with 0.
-- Notice that the generation function listOf returns a pair containing
-- a list and the generator to produce the next part of the sequence. This
-- is then propogated to the next generator function.
generateMany =
  let initialGenerator = generator 0
      (first, generator') = listOf (int32Range (0, 9)) 5 initialGenerator
      (second, generator'') = listOf (int32Range (10, 19)) 5 generator'
      (third, _) = listOf (int32Range (20, 29)) 5 generator''
  in first ++ second ++ third


-- A basic data type
data Color = Red | Green | Blue

-- Generates random colors
color : Generator g -> (Color, Generator g)
color gen =
  let (i, gen') = int32Range (0, 2) gen
      cval = case i of
        0 -> Red
        1 -> Green
        2 -> Blue
  in (cval, gen')
     
-- Generates a list of 5 random colors     
generateColorList = listOf color 5 (generator 42)

--main = flow down [asText generateFloat, asText generateIntList, asText generateMany]