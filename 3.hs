{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad
import System.Environment
import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> (Int, Int, S.Set (Int, Int))
readInput xs = (length (head $ lines xs), length xs,
  S.unions $ zipWith (\l j -> S.fromList $ map (\(_, i) -> (i, j)) $ filter ((== '#') . fst) $ zip l [0..]) (lines xs) [0..])

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 (w, h, m) = sum [1 | i <- [0..h], (3 * i `mod` w, i) `S.member` m]

solve2 :: _ -> _
solve2 (w, h, m) = product [sum [1 | i <- [0..h], (sx * i `mod` w, sy * i) `S.member` m] | (sx, sy) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]
