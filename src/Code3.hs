module Code3 where

import           Control.Lens.Cons
import           Control.Lens.Fold
import           Data.Bits
import           Data.Char
import           Data.List         hiding (dropWhile)
import           Data.List.Split
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Ord
import           Data.Text         (pack, strip, unpack)
import           Prelude           hiding (dropWhile)



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



-- https://www.codewars.com/kata/decode-the-morse-code/train/haskell
morseCodes :: M.Map String String
morseCodes = M.fromList [("-","T"),("--","M"),("---","O"),("-----","0"),("----.","9"),("---..","8"),("---...",":"),("--.","G"),("--.-","Q"),("--..","Z"),("--..--",","),("--...","7"),("-.","N"),("-.-","K"),("-.--","Y"),("-.--.","("),("-.--.-",")"),("-.-.","C"),("-.-.--","!"),("-.-.-.",";"),("-..","D"),("-..-","X"),("-..-.","/"),("-...","B"),("-...-","="),("-....","6"),("-....-","-"),(".","E"),(".-","A"),(".--","W"),(".---","J"),(".----","1"),(".----.","'"),(".--.","P"),(".--.-.","@"),(".-.","R"),(".-.-.","+"),(".-.-.-","."),(".-..","L"),(".-..-.","\""),(".-...","&"),("..","I"),("..-","U"),("..---","2"),("..--.-","_"),("..--..","?"),("..-.","F"),("...","S"),("...-","V"),("...--","3"),("...---...","SOS"),("...-..-","$"),("....","H"),("....-","4"),(".....","5")]

decodeMorse :: String -> String
decodeMorse str = unwords $ concatMap (\st -> morseCodes M.! st) . splitOn " " <$> splitOn "   " (unpack $ strip $ pack str)



-- https://www.codewars.com/kata/fizz-buzz-cuckoo-clock/train/haskell
fizzBuzzCuckooClock :: String -> String
fizzBuzzCuckooClock time
  | m == 30 = "Cuckoo"
  | m == 0 = unwords $ replicate (if h > 12 then h - 12 else if h == 0 then 12 else h) "Cuckoo"
  | m `mod` 15 == 0 = "Fizz Buzz"
  | m `mod` 5 == 0 = "Buzz"
  | m `mod` 3 == 0 = "Fizz"
  | otherwise = "tick"
  where
    [h, m] = (\x -> read x :: Int) <$> splitOn ":" time



-- https://www.codewars.com/kata/excel-sheet-column-numbers/train/haskell
titleToNb :: String -> Integer
titleToNb title = sum $ zipWith (\a b -> toInteger (ord a - 64) * toInteger 26 ^ b) title (reverse [0..length title - 1])



-- https://www.codewars.com/kata/8-towers/train/haskell
towerCombination :: Integer -> Integer
towerCombination n = product [1..n]



-- https://www.codewars.com/kata/how-many-twos/train/haskell
twoCount :: Int -> Int
twoCount n = go n 0
  where
    go m c
      | even m = go (m `div` 2) (c + 1)
      | otherwise = c



-- https://www.codewars.com/kata/thinkful-string-drills-repeater/train/haskell
repeater :: String -> Int -> String
repeater string n = concat $ replicate n string



-- https://www.codewars.com/kata/how-many-are-smaller-than-me/train/haskell
smaller :: Ord a => [a] -> [Int]
smaller xs = zipWith (\x i -> length $ filter (<x) $ drop i xs ) xs [0..]



-- https://www.codewars.com/kata/the-dropwhile-function/train/haskell
dropWhile :: [a] -> (a -> Bool) -> [a]
dropWhile [] _     = []
dropWhile (x:xs) p = if p x then dropWhile xs p else x:xs


-- https://www.codewars.com/kata/herons-formula-1/train/haskell
heron :: Double -> Double -> Double -> Double
heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2


-- https://www.codewars.com/kata/factorial-1/train/haskell
factorial :: Integer -> Integer
factorial n = product [1..n]



-- https://www.codewars.com/kata/exclamation-marks-series-number-3-remove-all-exclamation-marks-from-sentence-except-at-the-end/train/haskell
remove :: String -> String
remove s = filter (/='!') s ++ takeWhile (=='!') (reverse s)



-- https://www.codewars.com/kata/playing-with-digits/train/haskell
digpow :: Integer -> Integer -> Integer
digpow n p = if s `mod` n == 0 then s `div` n else -1
  where
    s = sum $ zipWith (\a b -> toInteger (digitToInt a) ^ b) (show n) [p..]



-- https://www.codewars.com/kata/equal-sides-of-an-array/train/haskell
findEvenIndex :: [Int] -> Int
findEvenIndex arr = fromMaybe (-1) $ elemIndex 0 $ fmap (\i -> sum (take i arr) - sum (drop (i + 1) arr)) [0..length arr - 1]



-- https://www.codewars.com/kata/build-a-pile-of-cubes/train/haskell
findNb :: Integer -> Integer
findNb m
    | div (root * (root + 1)) 2 ^ 2 == m = root
    | otherwise = -1
    where
        intSqrt = floor . sqrt . fromIntegral
        root = intSqrt (intSqrt m * 2)

-- findNb :: Integer -> Integer
-- findNb m = f 1
--   where
--     s i = (i * (i + 1)) ^ 2 `div` 4
--     f i
--       | s i > m = -1
--       | s i == m = i
--       | otherwise = f (i + 1)



-- https://www.codewars.com/kata/is-a-number-prime/train/haskell
isPrime :: Integer -> Bool
isPrime x
  | x `elem` [0, 1] = False
  | otherwise = null [i | i <- [2.. floor $ sqrt $ fromInteger x], x `mod` i == 0]



-- https://www.codewars.com/kata/convert-string-to-camel-case/train/haskell
toCamelCase :: String -> String
toCamelCase str = concat $ zipWith (\a b -> if b == 0 then a else capitalize a) (splitOneOf "_-" str) [0..]
  where
    capitalize :: String -> String
    capitalize ""     = ""
    capitalize (x:xs) = toUpper x : xs



-- https://www.codewars.com/kata/are-they-the-same/train/haskell
comp :: [Integer] -> [Integer] -> Bool
comp as bs
  | length as == length bs = null $ ((^2) <$> as) \\ bs
  | otherwise = False



-- https://www.codewars.com/kata/consecutive-strings/train/haskell
longestConsec :: [String] -> Int -> String
longestConsec strarr k =
  head $
  fromMaybe [""] $
  listToMaybe $
  reverse $
  groupBy (\a b -> length a == length b) $
  sortOn length $
  fmap concat $
  filter (\x -> length x == k) $
  take k <$> tails strarr



-- https://www.codewars.com/kata/does-my-number-look-big-in-this/train/haskell
narcissistic :: Integral n => n -> Bool
narcissistic n = fromIntegral n == sum (fmap (\i -> digitToInt i ^ length ns) ns)
  where
    ns = show $ fromIntegral n



-- https://www.codewars.com/kata/which-are-in/train/haskell
inArray :: [String] -> [String] -> [String]
inArray a1 a2 = sort $ nub $ filter (\s -> any (isInfixOf s) a2) a1
