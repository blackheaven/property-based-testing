module FizzBuzz where
import Data.Char(isDigit)
import Data.List(tails)

main = error "NE"

-- |
-- FizzBuzz.
--
-- prop> all (flip elem ['F', 'i', 'z', 'B', 'u', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '\n']) (fizzbuzz x)
-- prop> x >= 0 ==> length (lines (fizzbuzz x)) == x
-- prop> all (not . null) (linedFb x)
-- prop> all (\l -> isNumber l || elem l ["Fizz", "Buzz", "FizzBuzz"]) (linedFb x)
-- prop> all (\(i, v) -> not (isThird i) || contains "Fizz" v) (indexedFb x)
-- prop> all (\(i, v) -> not (isFifth i) || contains "Buzz" v) (indexedFb x)
-- prop> all (\(i, v) -> (isThird i || isFifth i) || isNumber v) (indexedFb x)
-- prop> all (\(i, v) -> (isThird i || isFifth i) || show i == v) (indexedFb x)
-- prop> all (\(i, v) -> (isThird i && isFifth i) || not (contains "FizzBuzz" v)) (indexedFb x)
fizzbuzz :: Int -> String
fizzbuzz n = unlines $ map r [1..n]
  where r i =  case (isThird i, isFifth i) of
                 (True, True)   -> "FizzBuzz"
                 (True, False)  -> "Fizz"
                 (False, True)  -> "Buzz"
                 (False, False) -> show i

-- Helper
contains :: Eq a => [a] -> [a] -> Bool
contains s t = any (== s) (map (take (length s)) (tails t))

linedFb :: Int -> [String]
linedFb = lines . fizzbuzz

isNumber :: String -> Bool
isNumber = all isDigit

indexedFb :: Int -> [(Int, String)]
indexedFb x = zip [1..x] (linedFb x)

isThird :: Int -> Bool
isThird = (== 0) . flip mod 3

isFifth :: Int -> Bool
isFifth = (== 0) . flip mod 5
