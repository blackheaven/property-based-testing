module FizzBuzz where
import Data.Maybe(mapMaybe)

main = error "NE"

data FizzBuzzResult = Number { extractNumber :: StrictlyPositive Int }
                    | Fizz
                    | Buzz
                    | FizzBuzz
                    deriving Eq

-- |
-- FizzBuzz.
--
-- prop> assert x $ (== x) . length
-- prop> assertIndexed x $ all (\(i, n) -> Just n == fmap fizzbuzzForIndex (mkStrictlyPositive i))
fizzbuzz :: StrictlyPositive Int -> [FizzBuzzResult]
fizzbuzz (StrictlyPositive n) = map fizzbuzzForIndex $ mapMaybe mkStrictlyPositive [1..n]

-- |
-- FizzBuzz for a given index.
--
-- prop> assertIndex x (notDivisibleBy x 3)  (not . isNumber)
-- prop> assertIndex x (notDivisibleBy x 5)  (not . isNumber)
-- prop> assertIndex x (notDivisibleBy x 3)  containsFizz
-- prop> assertIndex x (divisibleBy x 3)     (not . containsFizz)
-- prop> assertIndex x (notDivisibleBy x 5)  containsBuzz
-- prop> assertIndex x (divisibleBy x 5)     (not . containsBuzz)
-- prop> assertIndex x (notDivisibleBy x 15) (FizzBuzz ==)
-- prop> assertIndex x (divisibleBy x 15)    (FizzBuzz /=)
fizzbuzzForIndex :: StrictlyPositive Int -> FizzBuzzResult
fizzbuzzForIndex xe@(StrictlyPositive x)
 | divisibleBy x 15 = FizzBuzz
 | divisibleBy x  3 = Fizz
 | divisibleBy x  5 = Buzz
 | otherwise = Number xe
 
-- Helpers
newtype StrictlyPositive a = StrictlyPositive { getNumber :: a } deriving Eq

mkStrictlyPositive :: (Num a, Ord a) => a -> Maybe (StrictlyPositive a)
mkStrictlyPositive n = if n > 0 then Just (StrictlyPositive n) else Nothing

isNumber :: FizzBuzzResult -> Bool
isNumber n = case n of
               (Number _) -> True
               otherwise  -> False

assert :: Int -> ([FizzBuzzResult] -> Bool) -> Bool
assert n p
  | n > 0     = Just True == fmap (p . fizzbuzz) (mkStrictlyPositive n)
  | otherwise = True

assertIndexed :: Int -> ([(Int, FizzBuzzResult)] -> Bool) -> Bool
assertIndexed n p = assert n (p . zip [1..])

assertIndex :: Int -> Bool -> (FizzBuzzResult -> Bool) -> Bool
assertIndex n p r
  | n > 0     = p || Just True == fmap (r . fizzbuzzForIndex) (mkStrictlyPositive n)
  | otherwise = True

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy a b = mod a b == 0

notDivisibleBy :: Integral a => a -> a -> Bool
notDivisibleBy a b = not $ divisibleBy a b

containsFizz :: FizzBuzzResult -> Bool
containsFizz x = x == FizzBuzz || x == Fizz

containsBuzz :: FizzBuzzResult -> Bool
containsBuzz x = x == FizzBuzz || x == Buzz
