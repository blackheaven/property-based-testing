module FizzBuzz where

main = error "NE"

data FizzBuzzResult

-- |
-- FizzBuzz.
--
-- prop> x > 0 ==> Just x == fmap (length . fizzbuzz) (mkStrictlyPositive x)
fizzbuzz :: StrictlyPositive Int -> [FizzBuzzResult]
fizzbuzz = flip replicate undefined . getNumber

-- Helpers
newtype StrictlyPositive a = StrictlyPositive { getNumber :: a }

mkStrictlyPositive :: (Num a, Ord a) => a -> Maybe (StrictlyPositive a)
mkStrictlyPositive n = if n > 0 then Just (StrictlyPositive n) else Nothing
