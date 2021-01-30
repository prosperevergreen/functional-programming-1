-- A function nextIsGreater :: [Int] -> [Int] that, given a list of numbers, 
-- produces a list with all elements of the input list such that the element is 
-- followed by a greater number in the input list (the next number is greater).

nextIsGreater :: [Int] -> [Int]
nextIsGreater [] = []
nextIsGreater [x] = []
nextIsGreater (x:y) = [x | head y > x] ++ nextIsGreater y