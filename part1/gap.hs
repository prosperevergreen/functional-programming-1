-- Using recursion write a function gap :: (Char, Char) -> Int -> String -> Int that,
-- given a pair (c1,c2), a gap g and a string s returns an Int telling how many times (c1,c2) appear in s with gap g.

gap :: (Char, Char) -> Int -> String -> Int
gap (_, _) x y
  | length y <= 1 = 0
  | (x + 1) == length y = 0
gap (a, b) x y =
  ( if a == head y && b == y !! (x + 1)
      then 1
      else 0
  )
    + gap (a, b) x (tail y)