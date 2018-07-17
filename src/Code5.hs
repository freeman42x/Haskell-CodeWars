module Code5 where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Prelude         hiding (Either (..))
import           Text.Printf



stat :: String -> String
stat strg
  | null strg = ""
  | otherwise = "Range: " ++ range ++
               " Average: " ++ average ++
               " Median: " ++ median
  where
    inSeconds = hmsToSeconds <$> splitOn ", " strg
    [range, average, median] = fmap secondsToHms
      [maximum inSeconds - minimum inSeconds,
      sum inSeconds `div` length inSeconds,
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



-- https://www.codewars.com/kata/harshad-or-niven-numbers/train/haskell
isValid  :: Integer -> Bool
isValid number = number `mod` sumDigits == 0
  where
    sumDigits = sum $ read . pure <$> show number

getNext  :: Integer -> Integer
getNext n = head $ filter isValid [n + 1..]

getSerie :: Int -> Maybe Integer -> [Integer]
getSerie n start = take n $ filter isValid [st + 1..]
  where
    st = fromMaybe 0 start



-- https://www.codewars.com/kata/exercise-in-summing/train/haskell
minimumSum :: [Integer] -> Int -> Integer
minimumSum xs n = sum $ take n $ sort xs

maximumSum :: [Integer] -> Int -> Integer
maximumSum xs n = sum $ take n $ sortBy (flip compare) xs



-- https://www.codewars.com/kata/salesmans-travel/train/haskell
ad :: String
ad = "123 Main Street St. Louisville OH 43071,432 Main Long Road St. Louisville OH 43071,786 High Street Pollocksville NY 56432,\
\54 Holy Grail Street Niagara Town ZP 32908,3200 Main Rd. Bern AE 56210,1 Gordon St. Atlanta RE 13000,\
\10 Pussy Cat Rd. Chicago EX 34342,10 Gordon St. Atlanta RE 13000,58 Gordon Road Atlanta RE 13000,\
\22 Tokyo Av. Tedmondville SW 43098,674 Paris bd. Abbeville AA 45521,10 Surta Alley Goodtown GG 30654,\
\45 Holy Grail Al. Niagara Town ZP 32908,320 Main Al. Bern AE 56210,14 Gordon Park Atlanta RE 13000,\
\100 Pussy Cat Rd. Chicago EX 34342,2 Gordon St. Atlanta RE 13000,5 Gordon Road Atlanta RE 13000,\
\2200 Tokyo Av. Tedmondville SW 43098,67 Paris St. Abbeville AA 45521,11 Surta Avenue Goodtown GG 30654,\
\45 Holy Grail Al. Niagara Town ZP 32918,320 Main Al. Bern AE 56215,14 Gordon Park Atlanta RE 13200,\
\100 Pussy Cat Rd. Chicago EX 34345,2 Gordon St. Atlanta RE 13222,5 Gordon Road Atlanta RE 13001,\
\2200 Tokyo Av. Tedmondville SW 43198,67 Paris St. Abbeville AA 45522,11 Surta Avenue Goodville GG 30655,\
\2222 Tokyo Av. Tedmondville SW 43198,670 Paris St. Abbeville AA 45522,114 Surta Avenue Goodville GG 30655,\
\2 Holy Grail Street Niagara Town ZP 32908,3 Main Rd. Bern AE 56210,77 Gordon St. Atlanta RE 13000"

travel :: String -> String -> String
travel r zipcode = zipcode ++ ":" ++ intercalate "," (snd <$> ls) ++ "/" ++ intercalate "," (fst <$> ls)
  where
    ls = snd <$> filter (\(code, _) -> code == zipcode) lt
    lt = parse <$> splitOn "," r
    parse s = (unwords $ drop (l - 2) ws, (head ws, unwords $ drop 1 $ take (l - 2) ws))
      where
        ws = words s
        l = length ws



-- https://www.codewars.com/kata/moves-in-squared-strings-ii/train/haskell
rot :: String -> String
rot = intercalate "\n" . fmap reverse . reverse . lines

selfieAndRot :: String -> String
selfieAndRot strng = original ++ rotated
  where
    original = unlines $ (\s -> s ++ replicate len '.') <$> lines strng
    rotated = intercalate "\n" $ (\s -> replicate len '.' ++ s) <$> lines (rot strng)
    len = length $ lines strng

oper :: (String -> String) -> String -> String
oper = id



-- https://www.codewars.com/kata/prize-draw/train/haskell
rank :: String -> [Int] -> Int -> String
rank st we n
  | n > length we = "Not enough participants"
  | null st || st == "," = "No participants"
  | otherwise = res
  where
    rankS s = sum $ (\c -> ord (toLower c) - ord 'a' + 1) <$> s
    unweightedScore s = rankS s + length s
    weightedScores = zipWith (\a b -> (a, b * unweightedScore a)) (splitOn "," st) we
    sortedScores = sortBy sortScores weightedScores
    res = fst $ sortedScores !! (n - 1)

sortScores :: (Ord a, Ord a1) => (a, a1) -> (a, a1) -> Ordering
sortScores (name1, score1) (name2, score2)
  | score1 < score2 = GT
  | score1 > score2 = LT
  | otherwise = compare name1 name2
