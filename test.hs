module Test where

import Data.Type.Equality (apply)

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
triangles :: [(Integer, Integer, Integer)]
triangles = [(a, b, c) | a <- [0 .. 10], b <- [0 .. 10], c <- [0 .. 10], a ^ 2 + b ^ 2 == c ^ 2, a /= 0, b /= 0, c /= 0]

--- >>> triangles
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
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

head''' :: [a] -> Maybe a
head''' xs = case xs of
  [] -> Nothing
  (x : _) -> Just x

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

-- >>> 5 `myCmp` 5
-- EQ

isPythagora :: (Num a, Eq a) => (a, a, a) -> String
isPythagora (a, b, c)
  | a == 0 || b == 0 || c == 0 = "Cannot make a triangle if one side is 0 u fuck"
  | condition = "Did you know that Ancient philosophers took excessive amounts of psychodelic supstances?"
  | otherwise = "No, it is not in fact a pythagoric trangle"
  where
    condition = a ^ 2 + b ^ 2 == c ^ 2

--- >>> isPythagora (0,5,6)
--- >>> isPythagora (4,5,6)
-- "Cannot make a triangle if one side is 0 u fuck"
-- "No, it is not in fact a pythagoric trangle"
--- >>> isPythagora (3,4,5)
-- "Did you know that Ancient philosophers took excessive amounts of psychodelic supstances?"

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = first
    (l : _) = last

-- >>> initials "Haskell" "Curry"
-- "H. C."

calcDensities :: (RealFloat a) => [(a, a)] -> [a]
-- calcDensities xs = [density m v | (m, v) <- xs]
--   where
--     density m v = m / v
calcDensities xs = [density | (m, v) <- xs, let density = m / v]

-- >>> calcDensities [(1,5),(1,2)]
-- [0.2,0.5]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- >>>cylinder 2 5
-- 87.96459430051421

-- >>> [if 5>3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
-- ["Woo","Bar"]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Fuck off"
maximum' [x] = x
maximum' (x : xs)
  | x > maxElem = x
  | otherwise = maxElem
  where
    maxElem = maximum' xs

-- >>> maximum' [1,8,3,4,5]
-- 8

replicate' :: (Num a, Ord a) => a -> a -> [a]
replicate' x y
  | x <= 0 = []
  | otherwise = y : replicate' (x - 1) y

-- >>> replicate' 5 2
-- [2,2,2,2,2]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

-- >>> take' 3 [1,2,3,4]
-- [1,2,3]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- >>> reverse' [1,2,3]
-- [3,2,1]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- >>> zip' [1,2,3] [4,5,8]
-- [(1,4),(2,5),(3,8)]

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' elem (x : xs)
  | elem == x = True
  | otherwise = elem' elem xs

-- >>> elem' 1 [2,3,4,1]
-- True

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a >= x]
   in smallerSorted ++ [x] ++ biggerSorted

-- >>> quicksort [10,2,32,4,53,21,2,1]
-- [1,2,2,2,4,10,21,32,53]

-- max is a function which takes an element a which implements Ordered typeclass and returns a function that takes in a and returns a
-- >>> :t max
-- max :: Ord a => a -> a -> a

-- max 4 is a function which takes in a and returns a
-- >>> :t (max 4)
-- (max 4) :: (Ord a, Num a) => a -> a

-- max 4 5 is not a function, it's a value.
-- >>> :t (max 4) 5
-- (max 4) 5 :: (Ord a, Num a) => a

-- isn't currying fucking briliant?

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

--
-- >>> :t multThree
-- multThree :: Num a => a -> a -> a -> a
--
-- >>> :t multThree 3
-- multThree 3 :: Num a => a -> a -> a
--
-- >>> :t multThree 3 3
-- multThree 3 3 :: Num a => a -> a
--
-- >>> :t multThree 3 3 3
-- multThree 3 3 3 :: Num a => a

-- partial funcs
compareWithTen :: (Num a, Ord a) => a -> Ordering
compareWithTen = compare 10

-- >>> :t compareWithTen
-- compareWithTen :: (Num a, Ord a) => a -> Ordering
-- >>> compareWithTen 5
-- GT

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A' .. 'Z'])

--- >>> isUpperCase 'L'
-- True

doubleList :: (Num a) => [a] -> [a]
doubleList = map (* 2)

--- >>> doubleList [1,2,3]
-- [2,4,6]

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--- >>> applyTwice doubleList [1,2,3]
-- [4,8,12]
--

--- >>> zipWith (*) [1,2,3] [1,2,3]
-- [1,4,9]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- >>> zipWith' (*) [1,2,3] [1,2,3]
-- [1,4,9]
