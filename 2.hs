{-# LANGUAGE TypeApplications, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Arrow
import Control.Monad
import Data.List.Split
import System.Environment
import System.IO.Unsafe

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> [(Int, Int, Char, String)]
readInput = map ((\(((i,j),[a]), pw) -> (read i, read j, a, pw)) . first (first (splitOnP "-") . splitOnP " ") . splitOnP ": ") . lines
  where splitOnP s xs = case splitOn s xs of [a, b] -> (a, b)

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 = length . filter (\(i, j, a, pw) -> let l = length $ filter (== a) pw in i <= l && l <= j)

solve2 :: _ -> _
solve2 = length . filter (\(i, j, a, pw) -> (pw !! (i - 1) == a) /= (pw !! (j - 1) == a))
