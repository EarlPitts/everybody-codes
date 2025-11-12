module Day01 where

solve :: String -> Int
solve = sum . fmap parse
  where
    parse c = case c of
      'A' -> 0
      'B' -> 1
      'C' -> 3

solve' :: String -> Int
solve' = sum . fmap h . g
  where
    parse c = case c of
      'A' -> 0
      'B' -> 1
      'C' -> 3
      'D' -> 5
      _ -> 0
    g [] = []
    g (a : b : t) = [a, b] : g t
    h [m, m'] = case (m, m') of
      ('x', 'x') -> 0
      (_, 'x') -> parse m
      ('x', _) -> parse m'
      _ -> parse m + parse m' + 2

solve'' :: String -> Int
solve'' = sum . fmap h . g
  where
    parse c = case c of
      'A' -> 0
      'B' -> 1
      'C' -> 3
      'D' -> 5
      _ -> 0
    g [] = []
    g (a : b : c : t) = [a, b, c] : g t
    h [a, b, c] = case (a, b, c) of
      ('x', 'x', 'x') -> 0
      (_, 'x', 'x') -> parse a
      ('x', _, 'x') -> parse b
      ('x', 'x', _) -> parse c
      (_, _, 'x') -> parse a + parse b + 2
      (_, 'x', _) -> parse a + parse c + 2
      ('x', _, _) -> parse b + parse c + 2
      _ -> parse a + parse b + parse c + 6
