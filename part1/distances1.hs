-- a) Write a function distance1 :: String -> String -> Float that,
-- given two strings s1 and s2, calculates their distance using the following formula
-- ( (count of how many of the characters in s1 do not appear in s2 + (count of how many of the characters in s2 do not appear in s1) ) / ( (length of s1) + (length of s2) ).
-- If both of the lists are empty, then the distance is 0. For example, the distance between “aaabc” and “aabdd” with this function is (1 + 2) / (5 + 5).

notIn :: Char -> [Char] -> Bool
notIn char (strHead : strTail)
  | null (strHead : strTail) = True
  | char == strHead = False
  | otherwise = notIn char strTail -- char /= strHead

countNotIn :: [Char] -> [Char] -> Int
countNotIn (headStr1 : tailStr1) str2
  | null (headStr1 : tailStr1) = 0
  | otherwise = (if headStr1 `notIn` str2 then 1 else 0) + countNotIn tailStr1 str2

distance1 :: [Char] -> [Char] -> Float
distance1 str1 str2
  | null str1 && null str2 = 0
  | otherwise = fromIntegral (countNotIn str1 str2 + countNotIn str2 str1) / fromIntegral (length str1 + length str2)

-- b) Write a function distance2 :: String -> String -> Float that, given two strings s1 and s2,
-- calculates their distance using the following formula ( (count of characters in s1 that are other than any of ‘0’..‘9’) + (count of characters in s2 that are other that any of ‘0’..‘9’) ) / ( (length of s1) + (length of s2) ).
-- If both lists are empty, then the distance is 0. For example, the distance between “xy765” and “abc2311” with this function is (2 + 3) / (5 + 7).

notNum :: [Char] -> Int
notNum str
  | null str = 0
  | otherwise = (if head str `notElem` ['0' .. '9'] then 1 else 0) + notNum (tail str)

distance2 :: [Char] -> [Char] -> Float
distance2 str1 str2
  | null str1 && null str2 = 0
  | otherwise = fromIntegral (notNum str1 + notNum str2) / fromIntegral (length str1 + length str2)
