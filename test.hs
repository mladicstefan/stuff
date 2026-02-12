module Test where

factorial :: (Num x, Enum x) => x -> x
factorial x = product [1 .. x]

--- >>> factorial 5
-- 120

lucky :: (Integral a) => a -> String
lucky 7 = "You win!"
lucky x = "You lose!"

-- >>> lucky 5
-- "You lose!"
--
-- >>> lucky 7
-- "You win!"
--
triangles = [(a, b, c) | a <- [0 .. 10], b <- [0 .. 10], c <- [0 .. 10], a ^ 2 + b ^ 2 == c ^ 2]

--- >>> triangles
-- [(0,0,0),(0,1,1),(0,2,2),(0,3,3),(0,4,4),(0,5,5),(0,6,6),(0,7,7),(0,8,8),(0,9,9),(0,10,10),(1,0,1),(2,0,2),(3,0,3),(3,4,5),(4,0,4),(4,3,5),(5,0,5),(6,0,6),(6,8,10),(7,0,7),(8,0,8),(8,6,10),(9,0,9),(10,0,10)]
--

addVectors :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
addVectors (x, y, z) (p, q, s) = (x + p, y + q, z + s)

--- >>> addVectors (1,2,3) (1,2,3)
-- (2,4,6)

destructure :: [a] -> (a, a)
destructure (x : y : _) = (x, y)

--- >>> destructure [1,2,3]
-- (1,2)

--- >>>
head' :: [a] -> a
head' [] = error "Fuck off"
head' (x : _) = x

-- >>> head' [1,2,3]
-- 1
--
head'' :: [a] -> Maybe a
head'' [] = Nothing
head'' (x : _) = Just x

-- >>> head'' []
-- Nothing
-- >>> head'' [5,6,3,43,43]
-- Just 5

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- >>> length' [1,2,3,4]
-- 4

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- >>> sum' [1,2,3]
-- 6

capital :: String -> Maybe String
capital "" = Nothing
capital all@(x : xs) = Just ("The first letter of " ++ all ++ " is " ++ [x])

-- >>> capital "Belgrade"
-- Just "The first letter of Belgrade is B"

densityTell :: (RealFloat a) => a -> String
densityTell density
  | density < 1.2 = "Fly"
  | density <= 1000.0 = "Float on water"
  | otherwise = "SINK you motherfuck"

-- >>> densityTell 100.2
-- "Float on water"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- >>> max' 1 5
-- 5

myCmp :: (Ord a) => a -> a -> Ordering
myCmp a b
  | a < b = LT
  | a > b = GT
  | otherwise = EQ

-- >>> myCmp 5 5
-- EQ
