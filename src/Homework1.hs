module Homework1
  (
    toDigitsRev
  , toDigits
  , sumDigits
  , validate
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

