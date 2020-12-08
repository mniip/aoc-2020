{-# LANGUAGE PartialTypeSignatures, TypeApplications, BangPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad
import qualified Data.Set as S
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> [(String, Int)]
readInput = map ((\[a, b] -> (a, read $ dropWhile (== '+') b)) . words) . lines

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 xs = go 0 0 S.empty
  where
    go !ip !acc !seen
      | ip `S.member` seen = acc
      | otherwise = let seen' = S.insert ip seen in
          case xs !! ip of
            ("jmp", delta) -> go (ip + delta) acc seen'
            ("nop", _) -> go (ip + 1) acc seen'
            ("acc", delta) -> go (ip + 1) (acc + delta) seen'

solve2 :: _ -> _
solve2 xs = head [r | i <- [0..length xs - 1], Just r <- [run $ sub xs i]]
  where
    sub xs i = take i xs ++ mangle (xs !! i) : drop (i + 1) xs
    mangle ("jmp", d) = ("nop", d)
    mangle ("nop", d) = ("jmp", d)
    mangle ("acc", d) = ("acc", d)
    run xs = go 0 0 S.empty where
      go !ip !acc !seen
        | ip `S.member` seen = Nothing
        | ip >= length xs = Just acc
        | otherwise = let seen' = S.insert ip seen in
            case xs !! ip of
              ("jmp", delta) -> go (ip + delta) acc seen'
              ("nop", _) -> go (ip + 1) acc seen'
              ("acc", delta) -> go (ip + 1) (acc + delta) seen'
