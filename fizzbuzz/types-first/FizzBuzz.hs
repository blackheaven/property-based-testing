module FizzBuzz where
import Data.Maybe(mapMaybe, fromJust, isJust, isNothing)
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
-- prop> x > 0 ==> Just True == ((== x) . length . fizzbuzz <$> mkStrictlyPositive x)
fizzbuzz :: StrictlyPositive Int -> [FizzBuzzResult]
fizzbuzz (StrictlyPositive n) = map fizzbuzzForIndex $ mapMaybe mkStrictlyPositive [1..n]

-- |
-- FizzBuzz for a given index.
--
-- prop> x > 0 ==> isJust (fizzbuzzForIndex <$> mkStrictlyPositive x)
-- prop> x > 0 && notDivisibleBy x 15 ==> Just True == ((FizzBuzz /=) <$> (fmap fizzbuzzForIndex (mkStrictlyPositive x)))
-- prop> x > 0 ==> Just True == (containsFizz <$> (fmap fizzbuzzForIndex (mkStrictlyPositive (x * 3))))
-- prop> x > 0 ==> Just True == (containsBuzz <$> (fmap fizzbuzzForIndex (mkStrictlyPositive (x * 5))))
-- prop> x > 0 ==> Just True == ((FizzBuzz ==) <$> (fmap fizzbuzzForIndex (mkStrictlyPositive (x * 15))))
fizzbuzzForIndex :: StrictlyPositive Int -> FizzBuzzResult
fizzbuzzForIndex x = fromJust ((fizz x <> buzz x) <|> number x)

-- Rules
type FizzBuzzRule = Rule (StrictlyPositive Int) (FizzBuzzResult)

-- |
-- prop> x > 0 ==> assertRule (`divisibleBy` 3) fizz x
fizz :: FizzBuzzRule
fizz x = divisibleBy' x 3 =*> Fizz

-- |
-- prop> x > 0 ==> assertRule (`divisibleBy` 5) buzz x
buzz :: FizzBuzzRule
buzz x = divisibleBy' x 5 =*> Buzz

-- |
-- prop> x > 0 && notDivisibleBy x 5 ==> assertRule (`notDivisibleBy` 3) number x
-- prop> x > 0 && notDivisibleBy x 3 ==> assertRule (`notDivisibleBy` 5) number x
number :: FizzBuzzRule
number x = not (divisibleBy' x 3) && not (divisibleBy' x 5) =*> Number x

-- Helpers
newtype StrictlyPositive a = StrictlyPositive { getNumber :: a } deriving Eq

mkStrictlyPositive :: (Num a, Ord a) => a -> Maybe (StrictlyPositive a)
mkStrictlyPositive n = if n > 0 then Just (StrictlyPositive n) else Nothing

divisibleBy' :: Integral a => StrictlyPositive a -> a -> Bool
divisibleBy' (StrictlyPositive a) b = mod a b == 0

instance Monoid FizzBuzzResult where
    mempty = error "Mempty is not allowed on FizzBuzzResult"
    mappend Fizz Buzz = FizzBuzz

-- Rules system
type Rule i o = i -> Maybe o

(=*>) :: Bool -> a -> Maybe a
(=*>) = wrapMaybe
infix 2 =*>

-- Rules system helpers
wrapMaybe :: Bool -> a -> Maybe a
wrapMaybe p v = if p then Just v else Nothing

-- Test helpers
assertRule :: (Int -> Bool) -> FizzBuzzRule -> Int -> Bool
assertRule p r n = if p n
                     then isJust c
                     else isNothing c
                     where c = mkStrictlyPositive n >>= r

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy a b = mod a b == 0

notDivisibleBy :: Integral a => a -> a -> Bool
notDivisibleBy a b = not $ divisibleBy a b

containsFizz :: FizzBuzzResult -> Bool
containsFizz x = x == FizzBuzz || x == Fizz

containsBuzz :: FizzBuzzResult -> Bool
containsBuzz x = x == FizzBuzz || x == Buzz
