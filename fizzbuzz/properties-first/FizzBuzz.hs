module FizzBuzz where
import Data.Char(isDigit)
import Data.List(tails)

main = error "NE"

-- |
-- FizzBuzz.
--
-- prop> all (flip elem ['F', 'i', 'z', 'B', 'u', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '\n']) (fizzbuzz x)
-- prop> x >= 0 ==> length (lines (fizzbuzz x)) == x
-- prop> all (not . null) (lines $ fizzbuzz x)
-- prop> all (\l -> all isDigit l || elem l ["Fizz", "Buzz", "FizzBuzz"]) (lines $ fizzbuzz x)
-- prop> all (\(i, v) -> if mod i 3 == 0 then contains "Fizz" v else True) (zip [1..x] (lines $ fizzbuzz x))
-- prop> all (\(i, v) -> if mod i 5 == 0 then contains "Buzz" v else True) (zip [1..x] (lines $ fizzbuzz x))
fizzbuzz :: Int -> String
fizzbuzz n = unlines $ map (const "FizzBuzz") [1..n]

-- Helper
contains :: Eq a => [a] -> [a] -> Bool
contains s t = any (== s) (map (take (length s)) (tails t))
