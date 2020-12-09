{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Environment
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> [(String, [(String, Int)])]
readInput = map readLN . lines
  where
    readLN xs = case P.readP_to_S parser xs of ((r, _):_) -> r
    parser = liftA2 (,) (P.many P.get <* P.string " bags contain ") (liftA2 (:) bc (P.many (P.string ", " *> bc) <* P.string ".") <|> ([] <$ P.string "no other bags")) where bc = liftA2 (flip (,)) (P.readS_to_P reads <* P.string " ") (P.many P.get <* P.string " bag" <* P.optional (P.string "s"))

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> Int
solve1 xs = S.size (go (S.singleton "shiny gold")) - 1
  where
    go cs = if cs == cs' then cs else go cs'
      where cs' = S.union cs $ S.fromList [x | (x, ys) <- xs, not $ S.fromList (map fst ys) `S.disjoint` cs]

solve2 :: _ -> _
solve2 xs = go "shiny gold" -1 
  where
    go x = 1 + sum [i * go y | (y, i) <- fromJust $ lookup x xs]

