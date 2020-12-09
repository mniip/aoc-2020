{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> [(Int, Int)]
readInput = map readline . lines
  where
    readline :: String -> (Int, Int)
    readline xs = case span (`elem` "FB") xs of
      (r, c) -> ( foldl (\i l -> if l == 'B' then 2 * i + 1 else 2 * i) 0 r
                , foldl (\i l -> if l == 'R' then 2 * i + 1 else 2 * i) 0 c )

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> Int
solve1 xs = maximum $ map (\(r, c) -> r * 8 + c) xs

solve2 :: _ -> Int
solve2 xs = head $ filter (`notElem` seats) [mn .. mx]
  where
    seats =map (\(r, c) -> r * 8 + c) xs
    mn = minimum seats
    mx = maximum seats
