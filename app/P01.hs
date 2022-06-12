#!/usr/bin/env runghc

-- Find minimum free value

module Main (main) where

import Data.Array
import Data.Array (accumArray)
import GHC.Arr (elems)

main :: IO ()
main = do
  xs <- map read . words <$> getLine :: IO [Int]
  -- print $ minfreeFullSearch xs
  print $ minfreeSearch xs

-- | Calculation order: O(n^2)
minfreeFullSearch :: [Int] -> Int
minfreeFullSearch xs = head [x | x <- [0 .. n], x `notElem` xs]
  where
    n = length xs

-- | Filters [0..n] using the given array.
minfreeSearch :: [Int] -> Int
minfreeSearch = countHeadTrues . checklist
  where
    countHeadTrues = length . takeWhile id . elems

-- | Returns an arary in range [0..n] with their value True if the index is included in the input.
checklist :: [Int] -> Array Int Bool
checklist xs =
  accumArray
    -- applied to the values
    (||)
    False
    -- create array from range and (kev, value) pairs
    (0, n)
    (zip (filter (<= n) xs) (repeat True))
  where
    n = length xs

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
  where
    n = maximum xs
