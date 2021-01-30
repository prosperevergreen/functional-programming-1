-- A function headOrLast :: [String] -> Char -> [String] that, given a list of strings and a character, 
-- evaluates to a list with all the strings of the input list that either begin or end with the input character.

headOrLast :: Eq a => [[a]] -> a -> [[a]]
headOrLast x y = [ z | z <- x, head z == y || last z == y ]