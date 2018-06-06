{-# LANGUAGE LambdaCase #-}

module Code2 where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Text.Printf



twoDecimalPlaces :: Double -> Double
twoDecimalPlaces = (/100) . fromInteger . floor . (*100)



strongEnough :: [[Int]] -> Int -> String
strongEnough earthquake age
  | 1000 * (0.99 :: Double) ^^ age < fromIntegral magnitude = "Needs Reinforcement!"
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
alternateSqSum sequ
  | null sequ = Nothing
  | otherwise = Just $ sum $ zipWith (\f x -> f x) (cycle [id, (^ 2)]) sequ



rakeGarden :: String -> String
rakeGarden = unwords . fmap (\w -> if w == "rock" then "rock" else "gravel") . words



containAllRots :: String -> [String] -> Bool
containAllRots strng arr = all (`elem` arr) $ allRots strng

allRots :: String -> [String]
allRots s = fmap (`rotate` s) [0..length s - 1]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs



reverseByCenter :: String -> String
reverseByCenter xs = x2 ++ x1 ++ x0
  where first = length xs `div` 2
        mid = length xs `mod` 2
        x0 = take first xs
        x1 = take mid $ drop first xs
        x2 = drop (first + mid) xs



capitalize :: String -> [Int] -> String
capitalize s xs = zipWith (\c i -> if i `elem` xs then toUpper c else c) s [0..]



differenceOfSquares :: Integer -> Integer
differenceOfSquares n = sum [1..n] ^ 2 - sum [i ^ 2 | i <- [1..n]]



movie :: Int -> Int -> Double -> Int
movie card ticket perc = head $ dropWhile (\n -> sysA n <= sysB n) [1..]
  where
    sysA n = ticket * n
    sysB n = ceiling (fromIntegral card + fromIntegral ticket * ((perc ^ (n + 1) - perc) / (perc - 1)))



data Property = Property Bool   Bool  Bool deriving Show
                      -- prime  even  10*

numberProperty :: Integral n => n -> Property
numberProperty m = Property (isPrime m) (isEven m) (isMul10 m)
  where
    isEven n = n `mod` 2 == 0
    isMul10 n = n `mod` 10 == 0
    isPrime n
      | n < 2 = False
      | otherwise = null [ x | x <- [2..n-1], n `mod` x  == 0]


mainDiagonalProduct :: Num a => [[a]] -> a
mainDiagonalProduct mat = product $ zipWith (!!) mat [0..]



add :: (Num a, Enum a) => [a] -> a
add = sum . zipWith (*) [1..]



gcd :: Integral n => n -> n -> n
gcd x y = gcd_ (abs x) (abs y)
  where
    gcd_ a 0 = a
    gcd_ a b = gcd_ b (a `rem` b)



mean :: String -> (Double, String)
mean lst = (avg, str)
  where avg = (/ 10) $ fromIntegral $ sum $ map digitToInt $ filter isDigit lst
        str = filter isLetter lst



sumFromString :: String -> Integer
sumFromString n = sum $ snd $ foldl f ("",[0]) (n ++ " ")
  where f (s, nos) a =
          if isDigit a then (s ++ [a], nos)
                       else ("", if s /= "" then nos ++ [read s] else nos)



reverseFun :: String -> String
reverseFun s = foldl f s [0..length s - 1]
  where f acc i = take i acc ++ reverse (drop i acc)



longest :: [String] -> Int
longest = maximum . map length



alternateCase :: String -> String
alternateCase = map (\c -> if isUpper c then toLower c else toUpper c)



climb :: Int -> [Int]
climb y = go y []
  where go x ss
          | x == 1 = x : ss
          | otherwise = go (x `div` 2) (x : ss)



amIAfraid :: String -> Int -> Bool
amIAfraid dayOfTheWeek num =
  case dayOfTheWeek of
    "Monday"    -> num == 12
    "Tuesday"   -> num > 95
    "Wednesday" -> num == 34
    "Thursday"  -> num == 0
    "Friday"    -> even num
    "Saturday"  -> num == 56
    "Sunday"    -> abs num == 666
    _           -> error "Boom!"



fizzbuzz :: Int -> [Int]
fizzbuzz n = [ length $ filter (\i -> i `mod` 3 == 0 && i `mod` 5 /= 0) [1..n-1]
             , length $ filter (\i -> i `mod` 3 /= 0 && i `mod` 5 == 0) [1..n-1]
             , length $ filter (\i -> i `mod` 3 == 0 && i `mod` 5 == 0) [1..n-1]]



pattern' :: Int -> String
pattern' n
  | n < 1 = ""
  | otherwise = init $ unlines [ concatMap show [i..n] | i <- [1..n]]



filterNumbers :: String -> String
filterNumbers = filter (not . isDigit)



validateWord :: String -> Bool
validateWord word = all (== cf) $ tail cc
  where w = map toLower word
        cc = map (\c -> length $ filter (==c) w) $ nub w
        cf = head cc



zipValidate :: String -> Bool
zipValidate s = length (filter isDigit s) == 6 && head s `elem` "12346"



explode :: String -> String
explode = concatMap (\c -> replicate (read [c]) c)



stringCounter :: String -> Char -> Int
stringCounter inputS charS = length $ filter (== charS) inputS



-- https://www.codewars.com/kata/cartesian-neighbors/train/haskell
cartesianNeighbor :: Int -> Int -> [(Int,Int)]
cartesianNeighbor x y =
  [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
   (x - 1,     y),             (x + 1,     y),
   (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]



-- https://www.codewars.com/kata/first-class-function-factory/train/haskell
factory :: Int -> [Int] -> [Int]
factory x = map (*x)



-- https://www.codewars.com/kata/compoundarray/train/haskell
compoundArray :: [a] -> [a] -> [a]
compoundArray [] []         = []
compoundArray (x:xs) []     = x : compoundArray xs []
compoundArray [] (y:ys)     = y : compoundArray [] ys
compoundArray (x:xs) (y:ys) = x : y : compoundArray xs ys



-- https://www.codewars.com/kata/scoring-tests/train/haskell
scoreTest :: (Integral a) => [a] -> a -> a -> a -> a
scoreTest li a b c = sum $
  map (\case
          0 -> a
          1 -> b
          2 -> -c
          _ -> error "WAT!") li



-- https://www.codewars.com/kata/weight-of-its-contents/train/haskell
contentWeight :: Int -> String -> Int
contentWeight weight str = round $ (t * fromIntegral weight) / (1.0 + t)
  where t = parseTimes str

parseTimes :: String -> Double
parseTimes s = if bos == "larger" then times else 1 / times
  where ws = words s
        times = read $ head ws :: Double
        bos = last ws

-- t * b + b = f ---> b = f / (1 + t)
-- c = t * b ---> c = t * f / (1 + t)



-- https://www.codewars.com/kata/return-string-of-first-characters/train/haskell
makeString :: String -> String
makeString = map head . words



-- https://www.codewars.com/kata/alphabet-symmetry/train/haskell
solve :: [String] -> [Int]
solve = fmap sym
  where
    sym s = length $ filter (uncurry (==)) $ zip (fmap toLower s) ['a'..]



-- https://www.codewars.com/kata/number-pairs/train/haskell
getLargerNumbers :: Ord a => [a] -> [a] -> [a]
getLargerNumbers = zipWith max



-- https://www.codewars.com/kata/complete-the-pattern-number-3-horizontal-image-of-number-2/train/haskell
pattern :: Int -> String
pattern n = intercalate "\n" $ fmap (concatMap show) $ filter (not . null) $ inits [n,n-1..1]



-- https://www.codewars.com/kata/easy-wallpaper/train/haskell
wallpaper :: Double -> Double -> Double -> String
wallpaper l w h = sayNumber $ ceiling $ surfacePlus / rollSurface
  where
    surface = if l == 0 || w == 0 || h == 0 then 0 else 2 * (l * h + w * h)
    surfacePlus = surface * 1.15
    rollSurface = 5.2

sayNumber :: Int -> String
sayNumber n = ["zero", "one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
  "sixteen", "seventeen", "eighteen", "nineteen", "twenty"] !! n



-- https://www.codewars.com/kata/simple-beads-count/train/haskell
countRedBeads :: Int -> Int
countRedBeads 0 = 0
countRedBeads n = 2 * n - 2



-- https://www.codewars.com/kata/find-the-parity-outlier/train/haskell
findOutlier :: [Int] -> Int
findOutlier xs = head $ filter (if isEvens then odd else even) xs
  where
    isEvens = (length $ filter even xs) > (length $ filter odd xs)



-- https://www.codewars.com/kata/human-readable-time/train/haskell
humanReadable :: Int -> String
humanReadable x = printf "%02d:%02d:%02d" hours minutes seconds
  where
    hours = x `div` 3600
    minutes = (x - hours * 3600) `div` 60
    seconds = x - hours * 3600 - minutes * 60



-- https://www.codewars.com/kata/roman-numerals-encoder/train/haskell
solution :: Integer -> String
solution 0 = ""
solution n = v ++ solution (n - k)
  where
    (k, v) = fromJust $ find (\(key, _) -> key <= n) arabicToRoman
    arabicToRoman =
      [(1000, "M")
      ,(900, "CM")
      ,(500, "D")
      ,(400, "CD")
      ,(100, "C")
      ,(90, "XC")
      ,(50, "L")
      ,(40, "XL")
      ,(10, "X")
      ,(9, "IX")
      ,(5, "V")
      ,(4, "IV")
      ,(1, "I")]



-- https://www.codewars.com/kata/find-the-divisors/train/haskell
divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a = case [i | i <-[2..a `div` 2], a `mod` i == 0] of
              [] -> Left $ show a ++ " is prime"
              xs -> Right xs



-- https://www.codewars.com/kata/palindrome-chain-length/train/haskell
palindromeChainLength :: Integer -> Integer
palindromeChainLength n = go n 0
  where
    isPalindrome m = let s = show m in s == reverse s
    step m = let s = show m in m + (read $ reverse s)
    go m pcl = if isPalindrome m then pcl else go (step m) (pcl + 1)



-- https://www.codewars.com/kata/tube-strike-options-calculator/train/haskell
data Decision = Bus | Walk deriving (Eq, Show)
calculator :: Double   -- ^ the distance you would have to walk
           -> Double   -- ^ the distance the bus would travel
           -> Double   -- ^ the distance you have to *walk* to the bus
           -> Decision -- ^ your decision whether to take the Bus or Walk
calculator distance busDrive busWalk =
  if walkTime <= 2 && (walkTime < 1 / 6 || walkTime <= busTime) then Walk else Bus
  where
    walkTime = distance / 5
    busTime = busWalk / 5 + busDrive / 8



-- https://www.codewars.com/kata/the-fusc-function-part-1/train/haskell
fusc :: Int -> Int
fusc n
  | n == 0 = 0
  | n == 1 = 1
  | even n = fusc $ n `div` 2
  | otherwise = fusc (n `div` 2) + fusc (n `div` 2 + 1)



-- https://www.codewars.com/kata/filter-coffee/train/haskell
search :: Int -> [Int] -> String
search budget = intercalate "," . fmap show . sort . filter (<= budget)



-- https://www.codewars.com/kata/exclamation-marks-series-number-5-remove-all-exclamation-marks-from-the-end-of-words/train/haskell
remove :: String -> String
remove = unwords . fmap (reverse . dropWhile (=='!') . reverse) . words



-- https://www.codewars.com/kata/drying-potatoes/train/haskell
potatoes :: Int -> Int -> Int -> Int
potatoes p0 w0 p1 = w0 * (100 - p0) `div` (100 - p1)



-- https://www.codewars.com/kata/alphabetize-a-list-by-the-nth-character/train/haskell
sortIt :: [String] -> Int -> [String]
sortIt xs n = sortBy f xs
  where
    f a b
      | ai /= bi = ai `compare` bi
      | otherwise = a `compare` b
      where
        ai = g a
        bi = g b
        g = (!! (n - 1))



-- https://www.codewars.com/kata/character-concatenation/train/haskell
charConcat :: String -> String
charConcat word = go word 1
  where
    go [] _  = ""
    go [_] _ = ""
    go w n   = [head w] ++ [last w] ++ show n ++ go (init $ tail $ w) (n + 1)



-- https://www.codewars.com/kata/scaling-squared-strings/train/haskell
scale :: [Char] -> Int -> Int -> [Char]
scale strng k n = intercalate "\n" $ concatMap (replicate n) $ fmap (concatMap (replicate k)) $ (lines strng)



-- https://www.codewars.com/kata/figurate-numbers-number-2-pronic-number/train/haskell
isPronic :: Integer -> Bool
isPronic k = k == n * (n + 1)
  where
    n = floor $ sqrt $ fromIntegral k



-- https://www.codewars.com/kata/find-the-lucky-numbers/train/haskell
filterLucky :: [Int] -> [Int]
filterLucky = filter (('7' `elem`) . show)



-- https://www.codewars.com/kata/word-values/train/haskell
wordValue :: [[Char]] -> [Int]
wordValue = zipWith (\i s -> sumW s * i) [1..]
  where
    sumW = sum . fmap (\c -> if c == ' ' then 0 else ord c - ord 'a' + 1)



-- https://www.codewars.com/kata/count-consonants/train/haskell
consonantCount :: String -> Int
consonantCount = length . filter (\c -> isLetter c && (not $ c `elem` "aeiouAEIOU"))



-- https://www.codewars.com/kata/hamming-distance-part-1-binary-codes/train/haskell
hammingDistance :: String -> String -> Int
hammingDistance xs ys = sum $ zipWith (\a b -> if a == b then 0 else 1) xs ys
