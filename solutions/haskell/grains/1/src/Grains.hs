module Grains (square, total) where

smallestSquare :: Integer
smallestSquare = 1

largestSquare :: Integer
largestSquare = 64

square :: Integer -> Maybe Integer
square n
  | n >= smallestSquare && n <= largestSquare = Just (2^(n-1))
  | otherwise = Nothing

total :: Integer
total = 
  let numOrZero :: Maybe Integer -> Integer
      numOrZero (Just x)  = x
      numOrZero (Nothing) = 0
  in
  foldr (+) 0 (map (numOrZero . square) [smallestSquare..largestSquare])
