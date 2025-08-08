module DNA (toRNA) where

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
toRNA :: String -> Either Char String
toRNA =
  let toRNAhelper :: Char -> Either Char Char
      toRNAhelper 'G' = Right 'C'
      toRNAhelper 'C' = Right 'G'
      toRNAhelper 'T' = Right 'A'
      toRNAhelper 'A' = Right 'U'
      toRNAhelper x = Left x
  in
  mapM toRNAhelper
