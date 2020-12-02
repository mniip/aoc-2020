import Control.Applicative
import Control.Monad
import Data.Bits
import System.Environment
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> [(Int, Int, Char, String)]
readInput = map readLine . lines
  where
    readLine xs = case P.readP_to_S parser xs of [(x, [])] -> x
    parser = (,,,)
      <$> P.readS_to_P reads <* P.char '-'
      <*> P.readS_to_P reads <* P.char ' '
      <*> P.get <* P.string ": "
      <*> P.munch (const True)

printOutput :: Int -> IO ()
printOutput = print

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

solve1 :: [(Int, Int, Char, String)] -> Int
solve1 = count $ \(min, max, a, pw) -> let occur = count (== a) pw in min <= occur && occur <= max

solve2 :: [(Int, Int, Char, String)] -> Int
solve2 = count $ \(i, j, a, pw) -> let at k = pw !! (k - 1) == a in at i `xor` at j
