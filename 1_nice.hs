{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Maybe
import qualified Data.Set as S

main = do
  input <- readInput <$> getContents
  printOutput $ solve1 input
  printOutput $ solve2 input
  where
    readInput = map read . lines
    printOutput = print

solve1 :: [Int] -> Int
solve1 xs = head [i * (2020 - i) | i <- xs, (2020 - i) `S.member` p]
  where p = S.fromList xs

solve2 :: [Int] -> Int
solve2 xs = head $ [i * j * (2020 - i - j) | i <- xs, j <- xs, (2020 - i - j) `S.member` p]
  where p = S.fromList xs
