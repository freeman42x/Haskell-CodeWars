module Code1 where

import           Data.Char
import           Data.List
import           Debug.Trace
import           Numeric

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
