module Code1 where

import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord
import           Debug.Trace
import           Numeric
import           Text.Printf

remove :: String -> Int -> String
remove s 0 = s
remove (x:xs) n = if x == '!'
  then remove xs (n - 1)
  else x : remove xs n



accum :: String -> String
accum s = intercalate "-" $ map (\(c, i) -> toUpper c : replicate i (toLower c)) $ zip s [0..]



highAndLow :: String -> String
highAndLow input = show (maximum nos) ++ " " ++ show (minimum nos)
  where
    nos = map read (words input) :: [Int]



squareDigit :: Int -> Int
squareDigit n
  | n < 0 = read $ '-' : cm (tail $ show n)
  | otherwise = read $ cm $ show n
  where
    cm = concatMap (\c -> let v = digitToInt c in show $ v * v)



nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = nbh (percent / 100.0) aug p p0 0

nbh :: Double -> Int -> Int -> Int -> Int -> Int
nbh percent aug p pc y =
  if pc > p then y
  else nbh percent aug p pcn (y + 1)
  where
    pcn = pc + floor (fromIntegral pc * percent) + aug


-- ad n = adh n [] 1
--
-- adh n ds c
--   | n `mod` c == 0 = adh n (ds ++ c) (c + 1)
--   | otherwise = adh n ds (c + 1)



ad n = filter (\i -> n `mod` i == 0) [1..n]


collatzLength 1 = 1
collatzLength x = 1 + collatzLength (if even x then x `div` 2 else 3 * x + 1)



decimalToBinary 0 = "0"
decimalToBinary 1 = "1"
decimalToBinary n = if n `mod` 2 == 0 then "0" else "1" ++ decimalToBinary (n `div` 2)



-- import Numeric
-- main=do
-- [n,p]<-(map read.lines<$>getContents)::IO[Double]
-- print$truncate$logBase p n

-- Hlole

decode :: String -> String
decode ""     = ""
decode (x:xs) = x : decode (reverse xs)


maskify :: String -> String
maskify str = (\c -> if snd c < length str - 4 then '#' else fst c) <$> zip str [0..]



removeSmallest :: [Int] -> [Int]
removeSmallest [] = []
removeSmallest xss@(x:xs) = if x == m then xs else x : removeSmallest xs
  where m = minimum xss

-- SeriesSum(5) => 1 + 1/4 + 1/7 + 1/10 + 1/13 = "1.57"
seriesSum :: Integer -> String
seriesSum n = formatFloatN (round' (sum $ (\i -> 1 / i) <$> take (fromIntegral n) [1,4..]) 2) 2

round' :: Double -> Int -> Double
round' x n = fromIntegral (round (x * t)) / t
    where t = 10^n

formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""
-- printf "%.2f"

-- 1234567890
digit5 :: String -> Int
digit5 xs = maximum $ map (\s -> read s :: Int) $ take (length xs - 4) $ take 5 <$> tails xs

nbDig :: Int -> Int -> Int
nbDig n d = length $ filter (== intToDigit d) $ concat [show $ i ^ 2 | i <- [1..n]]

nbDigTest = nbDig 10 1


evaporator :: Double -> Double -> Double -> Integer
evaporator content evap_per_day threshold = fromIntegral
  $ length $ takeWhile (> threshold * 100) $ iterate (\x -> x * (100 - evap_per_day)/100) 100

divisors :: Int -> Int
divisors n = length [ i | i <- [1..n], n `mod` i == 0]


-- mxdiflg :: [String] -> [String] -> Maybe Int
-- mxdiflg s1 s2
--   | null s1 || null s2 = Nothing
--   | otherwise =
--     where



s1 = ["hoqq", "bbllkw", "oox", "ejjuyyy", "plmiis", "xxxzgpsssa", "xxwwkktt", "znnnnfqknaz", "qqquuhii", "dvvvwz"]
s2 = ["cccooommaaqqoxii", "gggqaffhhh", "tttoowwwmmww"]

-- max(abs(length(x) − length(y)))

mxdiflg :: [String] -> [String] -> Maybe Int
mxdiflg s1 s2
  | null s1 || null s2 = Nothing
  | otherwise = Just $ maximum $ (\(a,b) -> abs (length a - length b)) <$> liftA2(,) s1 s2

-- s 5 = [0, 1, 3, 6, 10, 15]

s :: Int -> [Int]
s n = fmap (\i -> i * signum n) $ take (abs n + 1) $ (\i -> sum [0..i]) <$> [0..]


sequenceSum :: Int -> String
sequenceSum n
  | n < 0 = show n ++ "<0"
  | n == 0 = "0 = 0"
  | otherwise = intercalate "+" (map show [0..6]) ++ " = " ++ show (sum [1..6])


data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)

arithmetic :: Fractional a => a -> a -> Operation -> a
arithmetic a b operator = case operator of
  Add      -> a + b
  Divide   -> a / b
  Multiply -> a * b
  Subtract -> a - b


isVampire :: Integer -> Integer -> Bool
isVampire a b = isVampire' xs ps
  where as = show a
        bs = show b
        xs = as ++ bs
        ps = show $ a * b

isVampire' :: String -> String -> Bool
isVampire' [] []     = True
isVampire' [] _      = True
isVampire' _ []      = False
isVampire' (f:fs) ts = f `elem` ts && isVampire' fs (delete f ts)


-- head :: [a] -> a
-- head (x:_) = x
--
-- tail :: [a] -> [a]
-- tail (_:xs) = xs
--
-- last :: [a] -> a
-- last = Code1.head . reverse
--
-- init :: [a] -> [a]
-- init = reverse . Code1.tail . reverse


findSum n = sum $ filter (\i -> i `mod` 3 == 0 || i `mod` 5 == 0) [0..n]


averages :: Maybe [Double] -> [Double]
averages mxs =
  case mxs of
    Nothing -> []
    Just xs -> averages' xs

averages' :: [Double] -> [Double]
averages' []  = []
averages' [x] = []
averages' xs  = zipWith (\a b -> (a + b) / 2) xs (tail xs)


replace old new = intercalate new . splitOn old


reverseLonger :: String -> String -> String
reverseLonger a b = shorter ++ reverse longer ++ shorter
  where
    shorter = if length a > length b then b else a
    longer = if length a > length b then a else b



pattern :: Int -> String
pattern n = intercalate "\n" $ map (\xs -> concat $ map show $ reverse xs) [[i..n] | i <- [1..n]]


fizzbuzz :: Int -> [String]
fizzbuzz n = fb <$> [1..n]

fb n
  | n `mod` 5 == 0 && n `mod` 3 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n


isSortedAndHow :: [Int] -> String
isSortedAndHow lst
  | sort lst == lst = "yes, ascending"
  | sort lst == reverse lst = "yes, descending"
  | otherwise = "no"


solution :: Int -> String
solution n = "Value is " ++ padd n 5

padd :: Int -> Int -> String
padd n d = last $ takeWhile (\i -> length i <= d) $ iterate ("0"++) (show n)



letters :: [(Char, String)]
letters =  [
    ('A', "Alpha"),  ('B', "Bravo"),   ('C', "Charlie"),
    ('D', "Delta"),  ('E', "Echo"),    ('F', "Foxtrot"),
    ('G', "Golf"),   ('H', "Hotel"),   ('I', "India"),
    ('J', "Juliett"),('K', "Kilo"),    ('L', "Lima"),
    ('M', "Mike"),   ('N', "November"),('O', "Oscar"),
    ('P', "Papa"),   ('Q', "Quebec"),  ('R', "Romeo"),
    ('S', "Sierra"), ('T', "Tango"),   ('U', "Uniform"),
    ('V', "Victor"), ('W', "Whiskey"), ('X', "X-ray"),
    ('Y', "Yankee"), ('Z', "Zulu")
  ]

nato :: String -> String
nato str = unwords $ (\c -> fromJust $ lookup (toUpper c) letters) <$> str



solution2 :: String -> String -> Int
solution2 xs x = length $ filter (\ys -> ys == x && length ys > 0) $ subLists xs

subLists :: [a] -> [[a]]
subLists xs = [] : concat [ divvy n 1 xs | n <- [1..length xs] ]



gps :: Int -> [Double] -> Int
gps s x = floor $ (3600 * delta_distance) / fromIntegral s
  where delta_distance = maximum $ zipWith (\a b -> b - a) (init x) (tail x)



partlist :: [String] -> [(String, String)]
partlist arr = [ (unwords $ take i arr, unwords $ drop i arr) | i <- [1..length arr - 1]]



candies :: [Int] -> Int
candies xs
  | length xs < 2 = -1
  | otherwise = sum $ (m -) <$> xs
  where m = maximum xs


isAscOrder :: [Int] -> Bool
isAscOrder xs = sort xs == xs



mostFrequentItemCount :: [Int] -> Int
mostFrequentItemCount [] = 0
mostFrequentItemCount xs = fst $ maximum $ fmap (\x -> (length x, head x)) $ group $ sort xs



hasUniqueChar :: String -> Bool
hasUniqueChar str = length str == (length $ nub str)



compare' :: Maybe String -> Maybe String -> Bool
compare' Nothing Nothing = True
compare' Nothing s2 = compare' (Just "") s2
compare' s1 Nothing = compare' s1 (Just "")
compare'(Just s1) (Just s2)
  = sumCode st1 == sumCode st2
    where sumCode s = foldr (\c acc -> acc + ord (toUpper c)) 0 s
          nillify s = if not $ all isAlpha s then "" else s
          st1 = nillify s1
          st2 = nillify s2



toCurrency :: Integer -> String
toCurrency price = reverse $ intercalate "," $ chunksOf 3 (reverse $ show price)



sortDict :: Ord v => [(k,v)] -> [(k,v)]
sortDict = sortBy (comparing (Down . snd))




dict = [
  (' ', ' '),
  ('A', '@'),
  ('B', '8'),
  ('C', '('),
  ('D', 'D'),
  ('E', '3'),
  ('F', 'F'),
  ('G', '6'),
  ('H', '#'),
  ('I', '!'),
  ('J', 'J'),
  ('K', 'K'),
  ('L', '1'),
  ('M', 'M'),
  ('N', 'N'),
  ('O', '0'),
  ('P', 'P'),
  ('Q', 'Q'),
  ('R', 'R'),
  ('S', '$'),
  ('T', '7'),
  ('U', 'U'),
  ('V', 'V'),
  ('W', 'W'),
  ('X', 'X'),
  ('Y', 'Y'),
  ('Z', '2')];

toLeetSpeak :: String -> String
toLeetSpeak = fmap (fromJust . (`lookup` dict))




vertMirror :: [Char] -> [Char]
vertMirror strng = intercalate "\n" $ map reverse $ splitOn "\n" strng

horMirror :: [Char] -> [Char]
horMirror strng = intercalate "\n" $ reverse $ splitOn "\n" strng

oper :: (String -> String) -> String -> String
oper fct strng = undefined



sunTriNumbers :: Integer -> Integer
sunTriNumbers n = sum $ scanl (+) 0 [1..n]



solution3 :: Integer -> Integer
solution3 number = sum $ filter (\i -> i `mod` 3 == 0 || i `mod` 5 == 0) [3..number - 1]



findOdd :: [Int] -> Int
findOdd xs = head $ fromJust $ find (\g -> odd $ length g) $ group $ sort xs


spinWords :: String -> String
spinWords str = unwords $ fmap (\s -> if length s > 4 then reverse s else s) $ words str



digitalRoot :: Integral a => a -> a
digitalRoot n = if s < 10 then s else digitalRoot s
  where s = groot n

groot :: Integral a => a -> a
groot n = fromIntegral $ sum $ fmap digitToInt $ show $ fromIntegral n



maxRot :: Integer -> Integer
maxRot n = maximum $ (read <$> scanl (\acc a -> (take a acc) ++ (rotate $ drop a acc)) ns [0..length ns - 1])
  where ns = show n

rotate :: String -> String
rotate (x:xs) = xs ++ [x]



oddCubed :: [Int] -> Int
oddCubed = sum . filter odd . fmap (^3)



swap :: String -> String
swap = fmap (\c -> if isUpper c then toLower c else toUpper c)



newAvg :: [Double] -> Double -> Maybe Int
newAvg xs navg = if n <= 0 then Nothing else Just n
  where n = round (navg * fromIntegral (length xs + 1) - sum xs)



notVisibleCubes :: Integer -> Integer
notVisibleCubes n = (n - 2) ^ 3



shadesOfGrey :: Int -> [String]
shadesOfGrey n
  | n < 1 = []
  | otherwise = fmap (\i -> fmap toLower $ printf "#%06X" (i * 65793) :: String) ([1..n] :: [Int])



-- isPowerOf4 :: Integral n => n -> Bool
-- isPowerOf4 n
--   | null tw = False
--   | otherwise = n == ((4^) $ last tw)
--   where tw = takeWhile (\i -> 4 ^ i <= n) [1..]


isPowerOf4 :: Integral n => n -> Bool
isPowerOf4 n
  | n == 1 = True
  | n `mod` 4 == 0 = isPowerOf4 $ n `div` 4
  | otherwise = False




numbersWithDigitInside :: Int -> Int -> [Int]
numbersWithDigitInside x d = [length xs, sum xs, if null xs then 0 else product xs]
  where xs = filter (\i ->  intToDigit d `elem` show i) [1..x]
