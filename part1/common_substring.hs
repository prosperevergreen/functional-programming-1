-- Write a function commonSubstring :: String -> String -> String that, given two strings s1 and s2, computes a common “substring” of s1 and s2 as follows.
-- The function finds the earliest common character c (closest to head of either s1 or s2 appearing in both sequences).
-- The function removes c and all the characters before it in both strings, puts c in the output string, and continues.
-- If there are two candidates for the earliest common character, pick the one from s1.

commonSubstring :: String -> String -> String
commonSubstring s1 s2
  | null s1 || null s2 = []
  | head s1 /= head s2 =
    if null [ch | ch <- s2, ch == head s1]
      then commonSubstring (tail s1) s2
      else commonSubstring s1 (tail s2)
  | head s1 == head s2 = head s1 : commonSubstring (tail s1) (tail s2)