{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad
import Data.Char
import Data.List.Split
import System.Environment
import System.Directory
import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

readInput :: String -> _
readInput = map (M.fromList . map (splitOnP ":") . words) . splitOn "\n\n"
  where splitOnP p xs = case splitOn p xs of [a,b] -> (a,b)

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 = length . filter (\m -> all (`M.member` m) ["iyr", "eyr", "hgt", "hcl", "ecl", "pid", "byr"])

solve2 :: _ -> _
solve2 = length . filter validate where
  validate m
    | Just sbyr <- M.lookup "byr" m
    , length sbyr == 4
    , [(byr, "")] <- reads sbyr
    , 1920 <= byr && byr <= 2002
    , Just siyr <- M.lookup "iyr" m
    , length siyr == 4
    , [(iyr, "")] <- reads siyr
    , 2010 <= iyr && iyr <= 2020
    , Just seyr <- M.lookup "eyr" m
    , length seyr == 4
    , [(eyr, "")] <- reads seyr
    , 2020 <= eyr && eyr <= 2030
    , Just shgt <- M.lookup "hgt" m
    , cmin <- reverse $ take 2 $ reverse shgt
    , shgn <- reverse $ drop 2 $ reverse shgt
    , [(hgn, "")] <- reads shgn
    , if cmin == "cm" then 150 <= hgn && hgn <= 193 else if cmin == "in" then 59 <= hgn && hgn <= 76 else False
    , Just ('#':hcl) <- M.lookup "hcl" m
    , length hcl == 6 && all (`elem` "0123456789abcdef") hcl
    , Just ecl <- M.lookup "ecl" m
    , ecl `elem` words "amb blu brn gry grn hzl oth"
    , Just pid <- M.lookup "pid" m
    , length pid == 9 && all isDigit pid
    = True
    | otherwise = False
