module Code3 where

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Ord



-- https://www.codewars.com/kata/sum-of-integers-in-string/train/haskell
sumOfIntegersInString :: String -> Int
sumOfIntegersInString n = sum $ snd $ foldl f ("",[0]) (n ++ " ")
  where f (s, nos) a =
          if isDigit a then (s ++ [a], nos)
                       else ("", if s /= "" then nos ++ [read s] else nos)



-- https://www.codewars.com/kata/replace-all-items/train/haskell
replaceAll :: Eq a => [a] -> a -> a -> [a]
replaceAll xs x y = fmap (\i -> if i == x then y else i) xs



-- https://www.codewars.com/kata/thinking-and-testing-a-and-b/train/haskell
testit :: Int -> Int -> Int
testit = (.|.)



-- https://www.codewars.com/kata/take-a-ten-minute-walk/train/haskell
isValidWalk :: String -> Bool
isValidWalk walk = length wtf == 10 && count 'n' == count 's' && count 'w' == count 'e'
  where count dir = length $ filter (==dir) wtf
        wtf = take 11 walk



-- https://www.codewars.com/kata/persistent-bugger/train/haskell
persistence :: Int -> Int
persistence n = go n 0
  where
    go m p
      | m `div` 10 == 0 = p
      | otherwise = go (step m) (p + 1)
      where
        step = product . fmap (\c -> read [c]) . show



-- https://www.codewars.com/kata/tribonacci-sequence/train/haskell
tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n tribs
  where
    tribs = a : b : c : zipWith (+) (zipWith (+) tribs (tail tribs)) (drop 2 tribs)



-- https://www.codewars.com/kata/create-phone-number/train/haskell
createPhoneNumber :: [Int] -> String
createPhoneNumber xs = "(" ++ a ++ ") " ++ b ++ "-" ++ c
  where
    a = map intToDigit $ take 3 xs
    b = map intToDigit $ take 3 $ drop 3 xs
    c = map intToDigit $ drop 6 xs



-- https://www.codewars.com/kata/counting-duplicates/train/haskell
duplicateCount :: String -> Int
duplicateCount = length . filter (\s -> length s > 1) . group . sort . map toLower



-- https://www.codewars.com/kata/your-order-please/train/haskell
yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy (comparing (head . filter isDigit)) . words
