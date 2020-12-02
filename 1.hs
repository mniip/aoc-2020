{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.Set as S
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput = map (read @Int) . lines

printOutput = print . head

solve1 :: [Int] -> [Int]
solve1 xs = let p = S.fromList xs in S.toList $ S.map (\i -> i * (2020 - i)) $ S.filter (\i -> (2020 - i) `S.member` p) p

solve2 :: [Int] -> [Int]
solve2 xs = let p = S.fromList xs in S.toList $ S.map (\(i, j) -> i * j * (2020 - j - i)) $ S.filter (\(i, j) -> (2020 - i - j) `S.member` p) $ S.cartesianProduct p p
