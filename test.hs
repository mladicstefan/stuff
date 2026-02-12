module Test where

factorial x = product [1 .. x]

-- >>> factorial 5
-- 120
--
lucky :: (Integral a) => a -> String
lucky 7 = "You win!"
lucky x = "You lose!"

-- >>> lucky 5
-- "You lose!"
--
-- >>> lucky 7
-- "You win!"
--
--
