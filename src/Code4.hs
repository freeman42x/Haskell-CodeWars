module Code4 where

import           Data.Array
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe



-- https://www.codewars.com/kata/two-joggers/train/haskell
nbrOfLaps :: Integer -> Integer -> (Integer, Integer)
nbrOfLaps bob charles = (d charles, d bob)
  where
    d n = n `div` gcd bob charles



-- https://www.codewars.com/kata/validate-credit-card-number/train/haskell
validate :: Integer -> Bool
validate n = (== 0) $ (`mod` 10) $ sum $ zipWith f (reverse $ show n) [0..]
    where
      f c i = if odd i then dd else d
        where
          d = digitToInt c
          d2 = 2 * d
          dd = if d2 > 9 then d2 - 9 else d2



-- https://www.codewars.com/kata/break-camelcase/train/haskell
solution :: String -> String
solution = unwords . split (startsWithOneOf ['A'..'Z'])



-- https://www.codewars.com/kata/find-the-missing-term-in-an-arithmetic-progression/train/haskell
findMissing :: Integral n => [n] -> n
findMissing xs = snd $ fromJust $ find (uncurry (/=)) $ zip xs [head xs, head xs + step..]
  where
    step = signum firstStep * min (abs firstStep) (abs secondStep)
    firstStep = xs !! 1 - head xs
    secondStep = xs !! 2 - xs !! 1



-- https://www.codewars.com/kata/reverse-or-rotate/train/haskell
revRot :: String -> Int -> String
revRot strng sz
  | sz <= 0 || null strng || sz > length strng = ""
  | otherwise = concatMap f $ filter (\ch -> length ch == sz) $ chunksOf sz strng
  where
    f str = if sc then reverse str else tail str ++ [head str]
      where
        sc = even $ sum $ fmap (\c -> digitToInt c ^ 3) str



-- https://www.codewars.com/kata/camelcase-method/train/haskell
camelCase :: String -> String
camelCase = concatMap (\s -> toUpper (head s) : tail s) . words



-- https://www.codewars.com/kata/multi-tap-keypad-text-entry-on-an-old-mobile-phone/train/haskell
presses :: String -> Int
presses s = sum $ fmap (\c -> keyPresses M.! toUpper c) s

keyPresses :: M.Map Char Int
keyPresses = M.fromList
  [('A',1)
  ,('B',2)
  ,('C',3)
  ,('D',1)
  ,('E',2)
  ,('F',3)
  ,('G',1)
  ,('H',2)
  ,('I',3)
  ,('J',1)
  ,('K',2)
  ,('L',3)
  ,('M',1)
  ,('N',2)
  ,('O',3)
  ,('P',1)
  ,('Q',2)
  ,('R',3)
  ,('S',4)
  ,('T',1)
  ,('U',2)
  ,('V',3)
  ,('W',1)
  ,('X',2)
  ,('Y',3)
  ,('Z',4)
  ,(' ',1)
  ,('1',1)
  ,('2',4)
  ,('3',4)
  ,('4',4)
  ,('5',4)
  ,('6',4)
  ,('7',5)
  ,('8',4)
  ,('9',5)
  ,('0',2)
  ,('*',1)
  ,('#',1)
   ]



-- https://www.codewars.com/kata/highest-scoring-word/train/haskell
high :: String -> String
high myStr
  | null myStr = ""
  | otherwise = snd $ fromJust $ find (\(v, _) -> v == fst (last lst)) lst
  where
    lst = sortOn fst $ (\w -> (value w, w)) <$> words myStr
    value = sum . fmap (\c -> ord c - ord 'a' + 1)



-- https://www.codewars.com/kata/word-a10n-abbreviation/train/haskell
abbreviate :: String -> String
abbreviate str = concatMap abbr $ split (whenElt (not . isLetter)) str

abbr :: String -> String
abbr s
  | length s > 3 = [head s] ++ show (length s - 2) ++ [last s]
  | otherwise = s



-- https://www.codewars.com/kata/longest-palindrome/train/haskell
longestPalindrome :: Eq a => [a] -> Int
longestPalindrome xss = length $ last $ sortOn length $ filter (\xs -> xs == reverse xs) $ allSublists xss
  where
    allSublists = concat . map tails . inits



-- https://www.codewars.com/kata/fibonacci-tribonacci-and-friends/train/haskell
xbonacci :: Num a => [a] -> Int -> [a]
xbonacci as n = go as
  where
    sz = length as
    go az
      | length az > n = take n az
      | length az == n = az
      | otherwise = go $ az ++ [sum $ drop (length az - sz) az]



-- https://www.codewars.com/kata/help-the-bookseller/train/haskell
data Stock = Stock String Int

stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist st cs
  | null st || null cs = []
  | otherwise = fmap (\c -> (c, m M.! c)) cs
  where
    m = M.unionsWith (+) (M.fromList.pure <$> (zip cs (repeat 0) ++ ((\(Stock c v) -> (head c, v)) <$> st)))



-- https://www.codewars.com/kata/playing-with-passphrases/train/haskell
playPass :: String  -> Int -> String
playPass s n = reverse $ zipWith (\g x -> g x) (cycle [toUpper, toLower]) (f <$> s)
  where
    f c
      | isLetter c = chr $ ord 'A' + ((ord c - ord 'A' + n) `mod` 26)
      | isDigit c = intToDigit $ 9 - digitToInt c
      | otherwise = c



-- https://www.codewars.com/kata/base-conversion/train/haskell
newtype Alphabet = Alphabet { getDigits :: [Char] } deriving (Show)
bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric :: Alphabet
bin = Alphabet $ "01"
oct = Alphabet $ ['0'..'7']
dec = Alphabet $ ['0'..'9']
hex = Alphabet $ ['0'..'9'] ++ ['a'..'f']
alphaLower    = Alphabet $ ['a'..'z']
alphaUpper    = Alphabet $ ['A'..'Z']
alpha         = Alphabet $ ['a'..'z'] ++ ['A'..'Z']
alphaNumeric  = Alphabet $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

convert :: Alphabet -> Alphabet -> String -> String
convert (Alphabet a) (Alphabet b) x = convFrom10 (convTo10 (Alphabet a) x) (Alphabet b)

convTo10 :: Alphabet -> String -> Integer
convTo10 (Alphabet a) x = sum $
  zipWith (\p q -> (toInteger $ fromJust $ elemIndex p a) * ((toInteger $ length a) ^ q))
  x (reverse $ take (length x) [0..])

convFrom10 :: Integer -> Alphabet -> String
convFrom10 x (Alphabet b) = (\c -> b !! (fromInteger c)) <$> go x []
  where
    go :: Integer -> [Integer] -> [Integer]
    go 0 xs  = (if null xs then [0] else []) ++ xs
    go xx xs = go (xx `div` (toInteger $ length b)) ((xx `mod` (toInteger $ length b)) : xs)



-- https://www.codewars.com/kata/fold-an-array/train/haskell
foldList :: [Int] -> Int -> [Int]
foldList xs n = last $ take (n + 1) $ iterate foldOnce xs
  where
    foldOnce ys = zipWith (+) firstHalf secondHalf
      where
        firstHalf = take firstHalfLength ys
        secondHalf = reverse $ (if odd $ length ys then [0] else []) ++ drop firstHalfLength ys
        firstHalfLength = length ys `div` 2 + if odd $ length ys then 1 else 0



-- https://www.codewars.com/kata/buying-a-car/train/haskell
nbMonths :: Integer -> Integer -> Integer -> Double -> [Integer]
nbMonths startPriceOld startPriceNew savingperMonth percentLossByMonth =
  (\(_1, _2, _3, _, _5) -> [_5, round $ _2 + _3 - _1]) <$>
  fromJust $
  find (\(_1, _2, _3, _, _) -> _2 + _3 >= _1) $
  scanl (\(_1, _2, _3, _4, _5) a ->
      let
        perc = if a then _4 + 0.5 else _4
        decRatio = 1 - perc / 100
      in
        (_1 * decRatio, _2 * decRatio, _3 + fromIntegral savingperMonth, perc, _5 + 1))
    (fromIntegral startPriceNew, fromIntegral startPriceOld, 0, percentLossByMonth, 0) (cycle [False, True])



-- https://www.codewars.com/kata/a-rule-of-divisibility-by-13/train/haskell
thirt :: Integer -> Integer
thirt n
  | n == step n = n
  | otherwise = thirt $ step n
  where
    step m = toInteger $ sum $ zipWith (*) xs ys
      where
        xs = digitToInt <$> reverse (show m)
        ys = cycle [1, 10, 9, 12, 3, 4]



-- https://www.codewars.com/kata/sum-consecutives/train/haskell
sumConsecutives :: [Int] -> [Int]
sumConsecutives = fmap sum . group



-- https://www.codewars.com/kata/grouped-by-commas/train/haskell
groupByCommas :: Int -> String
groupByCommas = intercalate "," . groups . reverse . show

groups :: String -> [String]
groups str = go str []
  where
    go "" xs = xs
    go s xs  = go (drop 3 s) (reverse (take 3 s) : xs)



-- https://www.codewars.com/kata/find-the-mine/train/haskell
mineLocation :: Array (Int, Int) Int -> Maybe (Int, Int)
mineLocation = fmap (\((x, y), _) -> (x,y)) . find (\((_, _), x) -> x == 1) . assocs



-- https://www.codewars.com/kata/triangle-number-check/train/haskell
isTriangleNumber :: Integer -> Bool
isTriangleNumber number = root ^ 2 == n
  where
    n =  8 * number + 1
    root = floor $ sqrt $ fromIntegral n



-- https://www.codewars.com/kata/calculate-string-rotation/train/haskell
shiftedDiff :: String -> String -> Int
shiftedDiff a b = go a 0
  where
    go str s
      | str == b = s
      | s == length b = -1
      | otherwise = go (last str : init str) (s + 1)



-- https://www.codewars.com/kata/adding-ordinal-indicator-suffixes-to-numbers/train/haskell
numberToOrdinal :: Int -> String
numberToOrdinal n = show n ++ suffix
  where
    suffix
      | n == 0 = ""
      | (n `mod` 100) `elem` [11..13] = "th"
      | n `mod` 10 == 1 = "st"
      | n `mod` 10 == 2 = "nd"
      | n `mod` 10 == 3 = "rd"
      | otherwise = "th"



-- https://www.codewars.com/kata/palindrome-for-your-dome/train/haskell
isPalindrome :: String -> Bool
isPalindrome xs = rev s == s
  where
    s = toLower <$> filter isAlphaNum xs

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]



-- https://www.codewars.com/kata/rainfall/train/haskell
mean :: String -> String -> Double
mean twn strng = common twn strng avg

variance :: String -> String -> Double
variance twn strng = common twn strng var

common :: Num p => String -> String -> ([Double] -> p) -> p
common twn strng f = maybe (-1) f $ lookup twn (parse strng)

parse :: String -> [(String, [Double])]
parse data0 = f <$> lines data0
  where
    f s = let str = splitOn ":" s in (head str, g $ last str)
    g s = h <$> splitOn "," s
    h s = read $ last $ splitOn " " s :: Double

avg :: [Double] -> Double
avg = (/) <$> sum <*> length'

var :: [Double] -> Double
var list = summedElements / lengthX
  where
    lengthX = length' list
    summedElements = sum (map (\x -> (x - avg list) ^ 2) list)

length' :: [a] -> Double
length' = fromIntegral . length



-- https://www.codewars.com/kata/backwards-read-primes/train/haskell
backwardsPrime :: Integer -> Integer -> [Integer]
backwardsPrime start stop = [n | n <- [start..stop], isPrime n, isReversedPrime n, isNotAPalindrome n]
  where
    isReversedPrime n = isPrime (read $ reverse $ show n :: Int)
    isNotAPalindrome n = show n /= reverse (show n)

isPrime :: Integral a => a -> Bool
isPrime x = null [i | i <- [2.. floor $ sqrt $ fromIntegral x], x `mod` i == 0]
