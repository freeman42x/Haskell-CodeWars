module Code3 where

import           Data.Char



-- https://www.codewars.com/kata/sum-of-integers-in-string/train/haskell
sumOfIntegersInString :: [Char] -> Int
sumOfIntegersInString n = sum $ snd $ foldl f ("",[0]) (n ++ " ")
  where f (s, nos) a =
          if isDigit a then (s ++ [a], nos)
                       else ("", if s /= "" then nos ++ [read s] else nos)



-- https://www.codewars.com/kata/replace-all-items/train/haskell
replaceAll :: Eq a => [a] -> a -> a -> [a]
replaceAll xs x y = fmap (\i -> if i == x then y else i) xs
