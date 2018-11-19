module Code6 where

import           Data.List



-- https://www.codewars.com/kata/pizza-pieces/train/haskell
maxPizza :: Integer -> Maybe Integer
maxPizza n
  | n < 0 = Nothing
  | otherwise = Just $ (n ^ 2 + n + 2) `div` 2



-- https://www.codewars.com/kata/run-length-encoding/train/haskell
runLengthEncoding :: String -> [(Int, Char)]
runLengthEncoding = reverse . foldl f ([])
  where
    f acc@((count, ch):xs) c
      | ch == c = (count + 1, ch):xs
      | otherwise = (1, c) : acc
    f [] c = [(1, c)]
