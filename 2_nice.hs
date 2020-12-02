import Control.Monad
import Data.Bits
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> [(Int, Int, Char, String)]
readInput = map readLine . lines
  where
    readLine xs = case splitOnP ": " xs of
      (ys, pw) -> case splitOnP " " ys of
        (zs, [a]) -> case splitOnP "-" zs of
          (i, j) -> (read i, read j, a, pw)
    splitOnP sep xs = case splitOn sep xs of [a, b] -> (a, b)

printOutput :: Int -> IO ()
printOutput = print

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

solve1 :: [(Int, Int, Char, String)] -> Int
solve1 = count $ \(min, max, a, pw) -> let occur = count (== a) pw in min <= occur && occur <= max

solve2 :: [(Int, Int, Char, String)] -> Int
solve2 = count $ \(i, j, a, pw) -> let at k = pw !! (k - 1) == a in at i `xor` at j
