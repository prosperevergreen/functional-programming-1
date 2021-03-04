-- Let us number the smaller case characters from ‘a’ to ‘z’ with numbers starting from 1, that is, ‘a’ is given 1, ‘b’ is given number 2, etc.

-- Write two functions:

-- A function charsDivisibleBy :: Int -> [Char] that, given a number n, returns all the characters that have a number divisible by n.

-- A function charsProductOf :: [Int] -> [Char] that, given a list of numbers ns, returns all the characters that have a number that is a product of any two numbers in ns.

charsDivisibleBy :: Int -> [Char]
charsDivisibleBy number = [['a' .. 'z'] !! (index - 1) | number > 0, number < 27, index <- [1 .. 26], index `mod` number == 0]

charsProductOf :: [Int] -> [Char]
charsProductOf [] = []
charsProductOf [head] = []
charsProductOf (head : tail) = [['a' .. 'z'] !! ((head * tailIndex) - 1) | tailIndex <- tail, (head * tailIndex) < 27, (head * tailIndex) > 0] ++ charsProductOf tail