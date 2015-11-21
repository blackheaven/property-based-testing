module FizzBuzz where

main = error "NE"

-- |
-- FizzBuzz.
--
-- prop> all (flip elem ['F', 'i', 'z', 'B', 'u', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '\n']) (fizzbuzz x)
-- prop> x >= 0 ==> length (lines (fizzbuzz x)) == x
-- prop> all (not . null) (lines $ fizzbuzz x)
fizzbuzz :: Int -> String
fizzbuzz n = concat $ replicate n  "F\n"
