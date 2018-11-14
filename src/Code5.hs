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



-- https://www.codewars.com/kata/count-the-smiley-faces/train/haskell
countSmileys :: [String] -> Int
countSmileys = length . filter isSmiley

isSmiley :: String -> Bool
isSmiley [h, m, t] = isEyes h && isNose m && isMouth t
isSmiley [h, t]    = isEyes h && isMouth t
isSmiley _         = False

isEyes :: Char -> Bool
isEyes ':' = True
isEyes ';' = True
isEyes _   = False

isNose :: Char -> Bool
isNose '-' = True
isNose '~' = True
isNose _   = False

isMouth :: Char -> Bool
isMouth ')' = True
isMouth 'D' = True
isMouth _   = False



-- https://www.codewars.com/kata/data-reverse/train/haskell
dataReverse :: [Int] -> [Int]
dataReverse = concat . reverse . chunksOf 8



-- https://www.codewars.com/kata/make-the-deadfish-swim/train/haskell
parse :: String -> [Int]
parse = init . reverse . foldl fn [0]

fn :: [Int] -> Char -> [Int]
fn (x:xs) 'i' = x + 1: xs
fn (x:xs) 'd' = x - 1: xs
fn (x:xs) 's' = x ^ 2: xs
fn (x:xs) 'o' = x : x : xs
fn xs _       = xs



-- https://www.codewars.com/kata/mutual-recursion/train/haskell
f :: Int -> Int
f 0 = 1
f n = n - mns !! (fns !! (n - 1))

m :: Int -> Int
m 0 = 0
m n = n - fns !! (mns !! (n - 1))

fns :: [Int]
fns = fmap f [0..]

mns :: [Int]
mns = fmap m [0..]



-- https://www.codewars.com/kata/tank-truck/train/haskell
tankvol :: Int -> Int -> Int -> Int
tankvol h d vt = floor $ area * l
  where
    r = fromIntegral d / 2
    a = r - fromIntegral h
    theta = (*2) $ acos $ a / r
    area = 1/2 * (theta - sin theta) * r ^ 2
    l = fromIntegral vt / (pi * r ^ 2)



-- https://www.codewars.com/kata/piano-kata-part-1/train/haskell
blackOrWhiteKey :: Int -> String
blackOrWhiteKey keyPressCount = if ind `elem` [1, 4, 6, 9, 11] then "black" else "white"
  where
    ind = ((keyPressCount - 1) `mod` 88) `mod` 12



-- https://www.codewars.com/kata/decipher-this/train/haskell
decipherThis :: String -> String
decipherThis message = unwords $ decipher <$> words message

decipher :: String -> String
decipher "" = ""
decipher s = fc : decBody body
  where
    fc = chr $ read $ takeWhile isDigit s
    body = dropWhile isDigit s

decBody :: String -> String
decBody "" = ""
decBody [b] = [b]
decBody [h, l] = [l, h]
decBody body = [l] ++ ib ++ [h]
  where
    h = head body
    l = last body
    ib = tail $ init body



-- https://www.codewars.com/kata/collatz/train/haskell
collatz :: Int -> String
collatz n
  | n == 1 = "1"
  | otherwise = show n ++ "->" ++ collatz (fun n)

fun :: Int -> Int
fun n
  | even n = n `div` 2
  | otherwise = 3 * n + 1



-- https://www.codewars.com/kata/transform-to-prime/train/haskell
minimumNumber :: [Integer] -> Integer
minimumNumber n = nextPrime - sumn
  where
    sumn = sum n
    nextPrime = head $ filter isPrime [sumn..]
    isPrime x = null [i | i <- [2.. floor $ sqrt $ fromIntegral x], x `mod` i == 0]



-- https://www.codewars.com/kata/rotate-array-js/train/haskell
rotate :: (Show a, Eq a) => Int -> [a] -> [a]
rotate _ [] = []
rotate n a
 | n < 0 = a
 | otherwise = take l $ drop (l - n `mod` l) $ cycle a
    where
      l = length a



-- https://www.codewars.com/kata/reducing-by-steps/train/haskell
gcdi :: Integer -> Integer -> Integer
gcdi = gcd

lcmu :: Integer -> Integer -> Integer
lcmu = lcm

som :: Integer -> Integer -> Integer
som = (+)

maxi :: Integer -> Integer -> Integer
maxi = max

mini :: Integer -> Integer -> Integer
mini = min

operArray :: (Integer -> Integer -> Integer) -> [Integer] -> Integer -> [Integer]
operArray fct arr ini = reverse $ tail $ foldl func [ini] arr
  where
    func a c = [curr] ++ [curr] ++ tail a
      where
        curr = fct (head a) c



-- https://www.codewars.com/kata/sorting-on-planet-twisted-3-7/train/haskell
sortTwisted37 :: [Int] -> [Int]
sortTwisted37 arr = read . (mapc <$>) . show <$> (sort $ read . (mapc <$>) . show <$> arr :: [Int])

mapc :: Char -> Char
mapc '3' = '7'
mapc '7' = '3'
mapc c   = c



-- https://www.codewars.com/kata/maze-runner/train/haskell
mazeRunner :: [[Int]] -> String -> String
mazeRunner maze directions = mazeSolver maze directions (elementFind maze 2)

elementFind :: [[Int]] -> Int -> Maybe (Int, Int)
elementFind c t = listToMaybe [ (x,y) | (y,line) <- zip [0..] c, x <- elemIndices t line ]

mazeSolver :: [[Int]] -> String -> Maybe (Int, Int) -> String
mazeSolver maze directions position
  | isNothing position = "Lost"
  | wentOutside || tile == 1 = "Dead"
  | tile == 3 = "Finish"
  | otherwise = mazeSolver maze (tail directions) newPosition
    where
      pos = fromJust position
      x = fst pos
      y = snd pos
      tile = maze !! y !! x
      wentOutside = x < 0 || y < 0 || x >= width || y >= height
      height = length maze
      width = length $ head maze
      newPosition = if null directions then Nothing else newPos
      newPos = Just $ case head directions of
                            'N' -> (x, y - 1)
                            'E' -> (x + 1, y)
                            'S' -> (x, y + 1)
                            _   -> (x - 1, y)
