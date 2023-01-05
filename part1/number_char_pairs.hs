-- Let us number the smaller case characters from ‘a’ to ‘z’ with numbers starting from 1, that is, ‘a’ is given 1, ‘b’ is given number 2, etc.

-- Write two functions:

-- A function charsDivisibleBy :: Int -> [Char] that, given a number n, returns all the characters that have a number divisible by n.

-- A function charsProductOf :: [Int] -> [Char] that, given a list of numbers ns, returns all the characters that have a number that is a product of any two numbers in ns.

charsDivisibleBy :: Int -> [Char]
charsDivisibleBy number = [char | (num, char) <- zip [1 ..] ['a' .. 'z'], number /= 0 ,num `mod` number == 0]

charsProductOf :: [Int] -> [Char]
charsProductOf numList = myNub [['a' .. 'z'] !! (x * y - 1) | x <- numList, y <- numList, x /= y, x * y <= 26]

myNub :: (Eq a) => [a] -> [a]
myNub [] = []
myNub (x:xs) = x : myNub (filter (/= x) xs)