module Code6 where

import           Data.Char
-- import           Data.List



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



-- https://www.codewars.com/kata/lucas-numbers/train/haskell
lucasnum :: Int -> Integer
lucasnum n
 | n == 0 = 2
 | n == 1 = 1
 | n > 1 = (lucasnumPos !! (n - 1)) + (lucasnumPos !! (n - 2))
 | otherwise = (lucasnumNeg !! (-n - 1)) - (lucasnumNeg !! (-n))
 where
  lucasnumPos = lucasnum <$> [0..]
  lucasnumNeg = lucasnum <$> [1, 0..]



-- https://www.codewars.com/kata/up-and-down/train/haskell
arrange :: String -> String
arrange s = unwords $ arrange' (words s) (cycle [(>), (<)])

arrange' :: [String] -> [Int -> Int -> Bool] -> [String]
arrange' (x1:x2:xs) (f:fs)
  | length x1 `f` length x2 = upper x2 f : arrange' (x1 : xs) fs
  | otherwise = upper x1 f : arrange' (x2 : xs) fs
arrange' [xs] (f:_) = [upper xs f]
arrange' [] _ = []
arrange' [_] _ = []
arrange' (_:_:_) _ = []

upper :: (Num t2, Num t1, Functor f) => f Char -> (t1 -> t2 -> Bool) -> f Char
upper s f = (if f 0 1 then toUpper else toLower) <$> s
