module Day01 where

import AoC.Utils

solution :: [Int] -> Int
solution ns = sum $ zipWith (-) (diff <$> ns) ns
 where
  diff n = (ceiling $ sqrt (fromIntegral n)) ^ 2

main = do
  input <- filter ((/=) ',') <$> readInput
  print $ solution (fmap read (words input))
