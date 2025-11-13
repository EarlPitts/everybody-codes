{-# LANGUAGE BangPatterns #-}

module Day02 where

import AoC.Utils
import Data.List
import Data.List.Split

input =
  "WORDS:THE,OWE,MES,ROD,HER\n\n\
  \AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE"
  -- \THERE IS THE END"
  -- \POWE PO WER P OWE R"
  -- \THE FLAME SHIELDED THE HEART OF THE KINGS"

data Input = Input [String] [String] deriving (Show)

parse :: String -> Input
parse str = Input (splitOn "," $ drop 6 rune) (words sentence)
  where
    [rune, _, sentence] = lines str

solution :: Input -> Int
solution (Input runes candidates) =
  sum $ fmap runicCount candidates
  where
    runicCount c = sum $ fmap (f c) runes
    f c r = (length $ splitOn r c) - 1

-- solution :: Input -> Int
-- solution (Input runes candidates) =
--   length $ filter isRunic runes
--   where
--     isRunic r = any (\c -> isInfixOf r c) candidates

main :: IO ()
main = do
  input <- parse <$> readInput
  print input
  -- print $ solution (parse input)
  print $ solution input
