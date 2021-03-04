-- Write a function distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
-- that, given a distance function f, a Float d, a String s and a list of Strings ss, returns all the strings in ss that are at most d distance away from s.


distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y

distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter func d s ss = [subss | subss <- ss, d >=  func s subss]