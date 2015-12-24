module FizzBuzz where
import Data.Maybe(mapMaybe, fromJust)
import Data.Monoid((<>))
import Control.Applicative((<|>))

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
fizzbuzzForIndex x = fromJust ((fizz x <> buzz x) <|> number x)

type Rule = StrictlyPositive Int -> Maybe FizzBuzzResult

fizz :: Rule
fizz x = if divisibleBy' x 3 then Just Fizz else Nothing

buzz :: Rule
buzz x = if divisibleBy' x 5 then Just Buzz else Nothing

number :: Rule
number x = Just $ Number x


divisibleBy' :: Integral a => StrictlyPositive a -> a -> Bool
divisibleBy' (StrictlyPositive a) b = mod a b == 0

instance Monoid FizzBuzzResult where
    mempty = error "Mempty is not allowed on FizzBuzzResult"
    mappend Fizz Buzz = FizzBuzz

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
