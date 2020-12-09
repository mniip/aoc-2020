{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad
import qualified Data.Set as S
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> [[S.Set Char]]
readInput = map (map S.fromList . lines) . splitOn "\n\n"

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 xss = sum [ S.size $ S.unions xs | xs <- xss]

solve2 :: _ -> _
solve2 xss = sum [ S.size $ foldr1 S.intersection xs | xs <- xss]
