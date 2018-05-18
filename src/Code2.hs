module Code2 where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe



twoDecimalPlaces :: Double -> Double
twoDecimalPlaces = (/100) . fromInteger . floor . (*100)



strongEnough :: [[Int]] -> Int -> String
strongEnough earthquake age
  | 1000 * 0.99 ^^ age < fromIntegral magnitude = "Needs Reinforcement!"
  | otherwise =  "Safe!"
    where magnitude = product $ fmap sum earthquake



isDivisible :: Integral n => n -> [n] -> Bool
isDivisible n = all (\x -> n `mod` x == 0)



filterString :: String -> Int
filterString = read . filter isDigit



product' :: String -> Int
product' s = len '!' * len '?'
  where len c = length $ filter (== c) s



angle::Int -> Int
angle n = (n - 2) * 180



sillyCASE :: String -> String
sillyCASE xs = map (\(c, i) -> if i < (length xs + 1) `div` 2
                                  then toLower c
                                  else toUpper c) $ zip xs [0..]



alternateSqSum :: [Integer] -> Maybe Integer
alternateSqSum seq
  | null seq = Nothing
  | otherwise = Just $ sum $ zipWith (\f x -> f x) (cycle [id, (^2)]) seq



rakeGarden :: String -> String
rakeGarden = unwords . fmap (\w -> if w == "rock" then "rock" else "gravel") . words



containAllRots :: String -> [String] -> Bool
containAllRots strng arr = all ((`elem` arr)) $ allRots strng

allRots :: String -> [String]
allRots s = fmap (`rotate` s) [0..length s - 1]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
