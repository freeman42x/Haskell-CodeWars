module Code5 where

import           Data.List
import           Data.List.Split
import           Prelude         hiding (Either (..))
import           Text.Printf



stat :: String -> String
stat strg
  | null strg = ""
  | otherwise = "Range: " ++ range ++
               " Average: " ++ average ++
               " Median: " ++ median
  where
    inSeconds = fmap hmsToSeconds $ splitOn ", " strg
    [range, average, median] = fmap secondsToHms
      [maximum inSeconds - minimum inSeconds,
      (sum inSeconds) `div` (length inSeconds),
      getMedian inSeconds]

secondsInHour :: Int
secondsInHour = 3600

secondsInMinute :: Int
secondsInMinute = 60

hmsToSeconds :: String -> Int
hmsToSeconds s = read hh * secondsInHour + read mm * secondsInMinute + read ss
  where
    [hh,mm,ss] = splitOn "|" s

secondsToHms :: Int -> String
secondsToHms sec = intercalate "|" $ fmap (printf "%02d") [hh, mm, ss]
  where
    (hh, rh) = divMod sec secondsInHour
    (mm, ss) = divMod rh secondsInMinute

getMedian :: [Int] -> Int
getMedian xs
  | odd lxs = xss !! (lxs `div` 2)
  | otherwise = (xss !! (lxs `div` 2) + xss !! (lxs `div` 2 - 1)) `div` 2
  where
    xss = sort xs
    lxs = length xs



-- https://www.codewars.com/kata/ackermann-function/train/haskell
ackermann :: Integer -> Integer -> Integer
ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | otherwise = ackermann (m - 1) (ackermann m (n - 1))



-- https://www.codewars.com/kata/exclamation-marks-series-number-17-put-the-exclamation-marks-and-question-marks-to-the-balance-are-they-balanced/train/haskell
data Comparison = Left | Right | Balance deriving (Show, Eq, Enum, Bounded)

balance :: String -> String -> Comparison
balance left right
  | bl > br = Left
  | bl == br = Balance
  | otherwise = Right
  where
    bl = bal left
    br = bal right

bal :: String -> Int
bal = sum . fmap (\c -> case c of
                          '!' -> 2
                          '?' -> 3
                          _   -> error "Nope!")



-- https://www.codewars.com/kata/are-we-alternate/train/haskell
isAlt :: String -> Bool
isAlt []       = True
isAlt [_]      = True
isAlt (x:y:xs) = if isVowel x == isVowel y then False else isAlt (y:xs)

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"
