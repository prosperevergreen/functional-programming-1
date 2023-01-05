module Bool3 (Bool3(False3,Unk3,True3), (&&&), (|||), not3) where

data Bool3 = False3 | Unk3 | True3 deriving (Eq,Show,Read) 

(&&&) :: Bool3 -> Bool3 -> Bool3
(&&&) x y
 | x == True3 && y == True3 = True3
 | x == False3 || y == False3 = False3
 |otherwise = Unk3

(|||) :: Bool3 -> Bool3 -> Bool3
(|||) x y
 | x == True3 || y == True3 = True3
 | x == False3 && y == False3 = False3
 |otherwise = Unk3

not3 :: Bool3 -> Bool3
not3 x
 | x == True3 = False3
 | x == False3 = True3
 |otherwise = Unk3