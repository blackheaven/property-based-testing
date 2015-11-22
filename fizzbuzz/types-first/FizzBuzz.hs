module FizzBuzz where
import Data.Maybe(mapMaybe)

main = error "NE"

data FizzBuzzResult = Number { extractNumber :: StrictlyPositive Int }

-- |
-- FizzBuzz.
--
-- prop> assert x $ (== x) . length
-- prop> assert x $  (all (\(i, n) -> not (isNumber n) || i == getNumber (extractNumber n))) . zip [1..n]
fizzbuzz :: StrictlyPositive Int -> [FizzBuzzResult]
fizzbuzz (StrictlyPositive n) = map Number $ mapMaybe mkStrictlyPositive [1..n]

-- Helpers
newtype StrictlyPositive a = StrictlyPositive { getNumber :: a }

mkStrictlyPositive :: (Num a, Ord a) => a -> Maybe (StrictlyPositive a)
mkStrictlyPositive n = if n > 0 then Just (StrictlyPositive n) else Nothing

isNumber :: FizzBuzzResult -> Bool
isNumber = const True

assert :: Int -> ([FizzBuzzResult] -> Bool) -> Bool
assert n p
  | n > 0     = Just True == fmap (p . fizzbuzz) (mkStrictlyPositive n)
  | otherwise = True
