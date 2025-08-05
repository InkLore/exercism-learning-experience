module DNA (toRNA) where

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
toRNA :: String -> Either Char String
toRNA =
  let toRNAhelper :: Char -> Either Char String -> Either Char String
      toRNAhelper x (Right ys)
        | x == 'G' = next 'C'
        | x == 'C' = next 'G'
        | x == 'T' = next 'A'
        | x == 'A' = next 'U'
        | otherwise = Left x
        where next x = Right $ x:ys
      toRNAhelper _ fail = fail
  in
  foldr toRNAhelper (Right [])

{-
import Data.Maybe (fromJust)
import Data.List (find)

toRNA :: String -> Either Char String
toRNA xs
  | xs == "" = Right ""
  | not (all ((flip elem) "GCTA") xs) = Left $ firstInvalid xs 
  | otherwise = Right $ map toRNAhelper xs
  where isDNAChar :: Char -> Bool
        isDNAChar x = not $ elem x "GCTA"
        firstInvalid :: String -> Char
        firstInvalid xs = fromJust $ find isDNAChar xs

toRNAhelper :: Char -> Char
toRNAhelper 'G' = 'C'
toRNAhelper 'C' = 'G'
toRNAhelper 'T' = 'A'
toRNAhelper 'A' = 'U'
toRNAhelper _ = error "Invalid DNA Base Pair"
-}