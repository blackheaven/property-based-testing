module FizzBuzz where
import Data.Maybe(mapMaybe)

main = error "NE"

data FizzBuzzResult = Number { extractNumber :: StrictlyPositive Int }
                    | Fizz
                    | Other
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
-- prop> assertIndex x $ \n -> notDivisibleBy x 3 || not (isNumber n)
-- prop> assertIndex x $ \n -> notDivisibleBy x 5 || not (isNumber n)
-- prop> assertIndex x $ \n -> notDivisibleBy x 3 || containsFizz n
-- prop> assertIndex x $ \n -> divisibleBy x 3    || not (containsFizz n)
fizzbuzzForIndex :: StrictlyPositive Int -> FizzBuzzResult
fizzbuzzForIndex xe@(StrictlyPositive x)
 | divisibleBy x 3 = Fizz
 | divisibleBy x 5 = Other
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

assertIndex :: Int -> (FizzBuzzResult -> Bool) -> Bool
assertIndex n p
  | n > 0     = Just True == fmap (p . fizzbuzzForIndex) (mkStrictlyPositive n)
  | otherwise = True

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy a b = mod a b == 0

notDivisibleBy :: Integral a => a -> a -> Bool
notDivisibleBy a b = not $ divisibleBy a b

containsFizz :: FizzBuzzResult -> Bool
containsFizz x = x == Fizz
