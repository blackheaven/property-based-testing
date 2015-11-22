module FizzBuzz where
import Data.Maybe(mapMaybe)

main = error "NE"

data FizzBuzzResult = Number { extractNumber :: StrictlyPositive Int }
                    | Other

-- |
-- FizzBuzz.
--
-- prop> assert x $ (== x) . length
-- prop> assertIndexed x $  (all (\(i, n) -> not (isNumber n) || i == getNumber (extractNumber n)))
-- prop> assertIndexed x $  (all (\(i, n) -> mod i 3 /= 0 || not (isNumber n)))
fizzbuzz :: StrictlyPositive Int -> [FizzBuzzResult]
fizzbuzz (StrictlyPositive n) = map fizzbuzzForIndex $ mapMaybe mkStrictlyPositive [1..n]
  where fizzbuzzForIndex xe@(StrictlyPositive x) = if mod x 3 == 0 then Other else Number xe

-- Helpers
newtype StrictlyPositive a = StrictlyPositive { getNumber :: a }

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
