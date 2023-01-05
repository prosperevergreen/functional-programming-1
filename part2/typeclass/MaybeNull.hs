module MaybeNull (MaybeNull(JustVal,Null)) where

-- MaybeNull:

data MaybeNull a = JustVal a | Null deriving (Eq,Show,Read)