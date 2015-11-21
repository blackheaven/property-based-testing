module FizzBuzz where
import Data.Char(isDigit)

main = error "NE"

-- |
-- FizzBuzz.
--
-- prop> all (flip elem ['F', 'i', 'z', 'B', 'u', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '\n']) (fizzbuzz x)
-- prop> x >= 0 ==> length (lines (fizzbuzz x)) == x
-- prop> all (not . null) (lines $ fizzbuzz x)
-- prop> all (\l -> all isDigit l || elem l ["Fizz", "Buzz", "FizzBuzz"]) (lines $ fizzbuzz x)
fizzbuzz :: Int -> String
fizzbuzz n = unlines $ map show [1..n]
