{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad
import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> _
readInput = map (read @Int) . lines

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 xs = go (reverse $ take 25 xs) (drop 25 xs)
  where
    go ps (x:xs)
      | x `elem` [x + y | (x:xs) <- tails ps, y <- ps]
      = go (take 25 $ x:ps) xs
      | otherwise = x

solve2 :: _ -> _
solve2 xs = head $ [minimum i + maximum i | t <- tails xs, i <- inits t, sum i == r]
  where
    r = solve1 xs
