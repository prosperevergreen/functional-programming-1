module Eq3 (Eq3,(===)) where 
import Bool3 ( Bool3(..), (&&&), (|||), not3)
import MaybeNull ( MaybeNull(..) )

class Eq3 a where  
    (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
    (===) x y
        | x == Unk3 || y == Unk3 = Unk3
        | x == y = True3
        |otherwise = False3

instance (Eq3 m) => Eq3 (MaybeNull m) where
    JustVal x === JustVal y = x === y   
    _ === _ = Unk3 