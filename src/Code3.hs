module Code3 where

import           Data.Bits
import           Data.Char
import           Data.List       hiding (dropWhile)
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Ord
import           Data.Text       (pack, strip, unpack)
import           Prelude         hiding (dropWhile)



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



-- https://www.codewars.com/kata/find-the-missing-letter/train/haskell
findMissingLetter :: String -> Char
findMissingLetter "" = error "should never happen"
findMissingLetter cs@(c:_) = snd $ fromJust $ find (uncurry (/=)) $ zip cs [c..]



-- https://www.codewars.com/kata/valid-braces/train/haskell
validBraces :: String -> Bool
validBraces xxs = go xxs ""
  where
    go [] ss
      | null ss = True
      | otherwise = False
    go (x:xs) []
      | x `elem` "{[(" = go xs [x]
      | otherwise = False
    go (x:xs) (s:ss)
      | x `elem` "{[(" = go xs (x:s:ss)
      | x `elem` ")]}" = s : [x] `elem` ["{}", "[]", "()"] && go xs ss
      | otherwise = False



-- https://www.codewars.com/kata/checking-groups/train/haskell
groupCheck :: String -> Bool
groupCheck xss = null $ foldl' f [] xss
  where
    f ('(':xs) ')' = xs
    f ('[':xs) ']' = xs
    f ('{':xs) '}' = xs
    f xs x         = x:xs



-- https://www.codewars.com/kata/delete-occurrences-of-an-element-if-it-occurs-more-than-n-times/train/haskell
deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n = foldl (\acc x -> if length (filter (==x) acc) >= n then acc else acc ++ [x]) [] lst



-- https://www.codewars.com/kata/weird-string-case/train/haskell
toWeirdCase :: String -> String
toWeirdCase = unwords . fmap f . words
  where
    f = zipWith (\a b -> if even a then toUpper b else toLower b) [0..]



-- https://www.codewars.com/kata/build-tower/train/haskell
buildTower :: Int -> [String]
buildTower n = fmap (\i -> replicate (n - i - 1) ' ' ++ replicate (2 * i + 1) '*' ++ replicate (n - i - 1) ' ') [0..n - 1]



-- https://www.codewars.com/kata/split-strings/train/haskell
solution1 :: String -> [String]
solution1 []       = []
solution1 [x]      = [x:"_"]
solution1 (x:y:ys) = (x:[y]) : solution1 ys



-- https://www.codewars.com/kata/sort-the-odd/train/haskell
sortArray :: [Int] -> [Int]
sortArray xs = snd $ foldl (\(oddf, res) a -> if even a then (oddf, res ++ [a]) else (tail oddf, res ++ [head oddf])) (oddfo, []) xs
  where
    oddfo = sort $ filter odd xs



-- https://www.codewars.com/kata/take-a-number-and-sum-its-digits-raised-to-the-consecutive-powers-and-dot-dot-dot-eureka/train/haskell
sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter p [a..b]
  where
    p n = n == sum (zipWith (\m i -> digitToInt m ^ i) (show n) [1..])



-- https://www.codewars.com/kata/tortoise-racing/train/haskell
race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g
  | v1 >= v2 = Nothing
  | otherwise = Just (h, m, s)
    where
      seconds = (3600 * g) `div` (v2 - v1)
      h = seconds `div` 3600
      m = (seconds - h * 3600) `div` 60
      s = seconds - h * 3600 - m * 60



-- https://www.codewars.com/kata/bouncing-balls/train/haskell
bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h bounce window
  | h > 0 && bounce > 0 && bounce < 1 && window < h = toInteger $ slength $ takeWhile (>= window) $ iterate (* bounce) h
  | otherwise = -1
    where
      slength x = (length x - 1) * 2 + 1



-- https://www.codewars.com/kata/roman-numerals-decoder/train/haskell
solution :: String -> Int
solution "" = 0
solution s = v + solution (drop (length k) s)
  where
    (k, v) = fromJust $ find (\(key, _) -> key == take (length key) s) romanToArabic
    romanToArabic =
      [("M", 1000)
      ,("CM", 900)
      ,("D", 500)
      ,("CD", 400)
      ,("C", 100)
      ,("XC", 90)
      ,("L", 50)
      ,("XL", 40)
      ,("X", 10)
      ,("IX", 9)
      ,("V", 5)
      ,("IV", 4)
      ,("I", 1)]



-- https://www.codewars.com/kata/write-number-in-expanded-form/train/haskell
expandedForm :: Int -> String
expandedForm n = intercalate " + " $
  filter (/= "0") $
  zipWith (\a b -> show $ digitToInt a * 10 ^ b) sn [length sn - 1, length sn - 2..0]
  where
    sn = show n



-- https://www.codewars.com/kata/detect-pangram/train/haskell
isPangram :: String -> Bool
isPangram str = ['a'..'z'] == sort (nub $ toLower <$> filter isLetter str)



-- https://www.codewars.com/kata/valid-phone-number/train/haskell
validPhoneNumber :: String -> Bool
validPhoneNumber s =
  length s == 14 &&
  head s == '(' &&
  isDigit (s !! 1) &&
  isDigit (s !! 2) &&
  isDigit (s !! 3) &&
  s !! 4 == ')' &&
  s !! 5 == ' ' &&
  isDigit (s !! 6) &&
  isDigit (s !! 7) &&
  isDigit (s !! 8) &&
  s !! 9 == '-' &&
  isDigit (s !! 10) &&
  isDigit (s !! 11) &&
  isDigit (s !! 12) &&
  isDigit (s !! 13)



-- https://www.codewars.com/kata/simple-encryption-number-1-alternating-split/train/haskell
encrypt :: String -> Int -> String
encrypt s i
  | i > 0 = iterate enc s !! i
  | otherwise = s

enc :: String -> String
enc s = str odd ++ str even
  where
    str f = fmap fst $ filter (\(_,i) -> f i) $ zip s [0..]

decrypt :: String -> Int -> String
decrypt s i
  | i > 0 = iterate dec s !! i
  | otherwise = s

dec :: String -> String
dec s = concat (zipWith (\a b -> b : [a]) (take lh s) (drop lh s)) ++ lst
  where
    lh = length s `div` 2
    lst = if odd $ length s then [last s] else ""



-- https://www.codewars.com/kata/reverse-polish-notation-calculator/train/haskell
data Term
  = Term Double
  | TermOp (Double -> Double -> Double)

calc :: String -> Double
calc s
  | s /= "" = head $ evalTerms $ map mkTerm $ words s
  | otherwise = 0

mkTerm :: String -> Term
mkTerm termStr = case termStr of
  "+" -> TermOp (+)
  "-" -> TermOp (-)
  "*" -> TermOp (*)
  "/" -> TermOp (/)
  _   -> Term $ read termStr

evalTerms :: [Term] -> [Double]
evalTerms = foldl modifyStack []
  where
    modifyStack stack term = case term of
      Term n -> n : stack
      TermOp op -> case stack of
        (a:b:_) -> op b a : drop 2 stack
        _       -> error "stack too small for operator application"



-- https://www.codewars.com/kata/rectangle-into-squares/train/haskell
squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect ol1 ol2
  | ol1 == ol2 = Nothing
  | otherwise = go ol1 ol2 []
  where
    go _ 0 r = Just r
    go l1 l2 r = go nl1 nl2 (r ++ [nl1])
      where
        nl1 = min l1 l2
        nl2 = abs (l1 - l2)



-- https://www.codewars.com/kata/pascals-triangle/train/haskell
pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat $ take n pascal

zapWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zapWith _ xs []         = xs
zapWith _ [] ys         = ys
zapWith f (x:xs) (y:ys) = f x y : zapWith f xs ys

extendWith :: (a -> a -> a) -> [a] -> [a]
extendWith _ []        = []
extendWith f xs@(x:ys) = x : zapWith f xs ys

pascal :: [[Int]]
pascal = iterate (extendWith (+)) [1]
