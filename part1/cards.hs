-- We represent playing cards with (Char, Int) pairs. ‘s’ means spades, ‘h’ hearts, ‘c’ clubs’ and ‘d’ diamonds, with number values going from 2 to 14 (Ace being 14). 
-- Consider a game, where a player is dealt two cards and wins credits based on the following rules:

-- If the player has the Ace of Spades (‘s’, 14), then the player wins 14 credits.
-- Otherwise if the player has two consecutive numbers of the same suit, then the player wins 8 credits.
-- Otherwise if the player has a pair (same number values), then the player wins 6 credits.
-- Otherwise if the player has to consecutive numbers, then the player wins 4 credits.
-- Otherwise if the player has two cards of the same suit, then the player wins 2 credits.
-- Otherwise, the player wins 0 credits.
-- Write a function credits :: (Char, Int) -> (Char, Int) -> Int that evaluates the given credits.

credits :: (Char, Int) -> (Char, Int) -> Int
credits (s1, n1) (s2, n2) 
    | s1 `notElem` ['s','h','c','d'] || s1 `notElem` ['s','h','c','d'] || n1 `notElem` [2..14] || n2 `notElem` [2..14] = error "wrong input value(s): credits (s1, n1) (s2, n2): s1,s2 elem ['s','h','c','d'] & n1,n2 elem [2..14]"
    | (s1 == 's' && n1 == 14) || (s2 == 's' && n2 == 14) = 14
    | abs(n1-n2) == 1 && s1 == s2 = 8
    | n1 == n2 && s1 /= s2 = 6
    | abs(n1-n2) == 1 = 4
    | s1 == s2 = 2

credits (s1, n1) (s2, n2) = 0

