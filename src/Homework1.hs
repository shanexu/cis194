module Homework1
  (
    toDigitsRev
  , toDigits
  , sumDigits
  , validate
  , hanoi
  , hanoi1
  , hanoi2
  ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev(n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (d : ds)
  | even (length ds) = d : doubleEveryOther ds
  | otherwise = (d * 2) : doubleEveryOther ds

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigitsRev) 0

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoi1 :: Integer -> Integer
hanoi1 n
  | n <= 0 = 0
  | otherwise = 2 * hanoi1 (n-1) + 1

-- haoni with 4 pegs
hanoi2 :: Integer -> Integer
hanoi2 1 = 1
hanoi2 2 = 3
hanoi2 3 = 5
hanoi2 4 = 9
hanoi2 n
  | n <= 0 = 0
  | otherwise = f n
  where
    hk k = 2 * hanoi2 (n - k) + hanoi1 k
    f 1 = hk 1
    f k = min (hk k) (f (k - 1))
