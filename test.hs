-- simple function takes in x and returns x + x;
-- call with: doubleMe 2 => 4
doubleMe :: Num a => a -> a
doubleMe x = x + x

-- functions that takes 3 arguments and returns their multiplies them
-- call with: multiplyUs 2 3 4 => 24
multiplyUs :: Num a => a -> a -> a -> a
multiplyUs x y z = x*y*z

-- function which and if statement
-- call with: doubleSmall 4 => 8
-- call with: doubleSmall 30 => 30
doubleSmall :: (Ord p, Num p) => p -> p
doubleSmall x = if x < 20 then 2*x else x

replicate' :: (Eq t, Num t) => t -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

take' :: (Eq t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' x (y:z) = y : take' (x-1) z


repeat' :: t -> [t]
repeat' x = x : repeat' x 

-- points z = [(x,y)| x<-[-z..z], y <-[-z..z], abs(x + y) <= z]

sum:: Integer -> Integer -> Integer
sum a b = a + b

len :: Show a => [a] -> [Char]
len ls@(l:_) = "List starts with " ++
    show l ++ " and is " ++
    show (length ls) ++ " items long."
len [] = "List is empty!"

data Color = C { red, green, blue :: Int }

instance Show Color where
    show C { red = a, green=b, blue=c } = "Color red=" ++ show a ++ ", green=" ++ show b ++ ", blue=" ++ show c

isGreenZero (C { green = 0 }) = True
isGreenZero _ = False