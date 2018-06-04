module Code0 where

import           Data.Char
import           Data.List
import           Data.List.Split

solve1 :: String -> String
solve1 = concat. reverse. split (oneOf "+*-/")



getIssuer :: Int -> String
getIssuer number
  | (isPrefixOf "34" no || isPrefixOf "37" no) && length no == 15 = "AMEX"
  | isPrefixOf "6011" no && length no == 16 = "Discover"
  | (isPrefixOf "51" no || isPrefixOf "52" no || isPrefixOf "53" no || isPrefixOf "54" no || isPrefixOf "55" no)
    && length no == 16 = "Mastercard"
  | isPrefixOf "4" no && ((length no == 13) || (length no == 16)) = "VISA"
  | otherwise = "Unknown"
    where no = show number


-- testit :: String -> String
-- testit xs = take taken prefix
--   where prefix = takeWhile (\c -> c == head xs) xs
--         taken = ceiling (fromIntegral (length prefix) / (fromIntegral 2))


-- testit :: String -> String
-- testit xs =
--   map (\(c, i) -> c) $
--   filter (\(c, i) -> i `mod` 2 == 1) (zip xs [1..])
--
-- testit :: String -> String
-- testit xs = take taken prefix
--   where prefix = takeWhile (\c -> c == head xs) xs
--         taken = ceiling (fromIntegral (length prefix) / (fromIntegral 2))

-- testit :: String -> String
-- testit xs =
--   takeWhile (\c -> c == head xs) $
--   map (\(c, i) -> c) $
--   filter (\(c, i) -> i `mod` 2 == 1) (zip xs [1..])

-- average of 2 letters?!


-- testit :: String -> String
-- testit "" = ""
-- testit (c:[]) = c:""
-- testit xs =
--   map (\(c, d, i) -> c) $
--   filter (\(c, d, i) -> i `mod` 2 == 1) $
--   zip3 xs (tail xs) [1..]


testit :: String -> String
testit "" = ""
testit [c] = c:""
testit xs = if length xs `mod` 2 == 1 then prefix ++ [last xs] else prefix
  where
    prefix =
      map (\(c, d, _) -> chr $ (ord c + ord d) `div` 2) $
      filter (\(_, _, i) -> odd (i :: Int)) $
      zip3 xs (tail xs) [1..]


-- solve :: String -> Bool
-- solve xs = any (\s -> s == reverse s) $ fmap (\i -> rotate i xs) [0..length xs - 1]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs


-- getFactors :: Int -> [Int]
-- getFactors 1 = [1]
-- getFactors n
--   | factors == []  = 1 : [n]
--   | otherwise = factors ++ getFactors (n `div` (head factors))
--   where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]



-- getFactors :: Int -> [Int]
-- getFactors n = [1] ++ getF n
--
-- getF 1 = []
-- getF n =
--   case factors of
--     [] -> [n]
--     _  -> factors ++ getF (n `div` (head factors))
--   where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n]



-- getFactors :: Int -> [Int]
-- getFactors n = [1] ++ getF n
--
-- getF 1 = []
-- getF n = filter (\x -> (n `mod` x) == 0) [2 .. n]



isPrime :: Integral a => a -> Bool
isPrime n = null [ x | x <- [2..n - 1], even n ]



-- foldr (\(Just x) (Just acc) -> Just (x + acc)) 0 [Just 2, Just 3]


daysRepresented :: [(Int,Int)] -> Int
daysRepresented =  foldr (\(x1, x2) acc -> acc + x2 - x1 + 1) 0


-- merge :: [(Int,Int)] -> [Bool]
-- merge xs = foldr (\(x1, x2) acc -> fmap (\day -> if ((acc !! day == True) || day `elem` [x1, x1 + 1..x2]) then True else False) allDays) [] xs
--
-- allDays = [1..365]


-- merge intervals into accumulator and at the end sum the lengths
-- the accumulator must contain no overlapping intervals

-- merge:
-- [1,2] [3,4] [4,5]
-- [1,7]
-- map 1..365 based on being set in any of the sets
-- acc is list of days / Integer
-- if in acc or in the new interval then map it to set in new acc

merge :: [(Int,Int)] -> [Bool]
merge = foldr (\(x1, x2) acc -> fmap (\day -> (acc !! (day - 1))
  || (day >= x1 && day <= x2)) allDays) allBool



allDays :: [Int]
allDays = [1..365]

allBool :: [Bool]
allBool = take 365 [False, False ..]


-- solve :: String -> Bool
-- solve xs = (length $ filter f $ zip3 xs (reverse xs) [0..]) == 1
--   where f (x, y, i) = i < limI && x /= y
--         limI = round $ ((fromIntegral $ length xs) / (fromIntegral 2))


-- solve :: String -> Bool
-- solve xs = (length $ filter f $ zip3 xs (reverse xs) [0..]) <= 1
--   where f (x, y, i) = i <= limI && x /= y
--         limI = (ceiling $ ((fromIntegral $ length xs) / (fromIntegral 2))) - 1


-- solve :: String -> Bool
-- solve xs
--   | (length xs `mod` 2 == 0) && (xs == reverse xs) = False
--   | otherwise = (length $ filter f $ zip3 xs (reverse xs) [0..]) <= 1
--   where f (x, y, i) = i <= limI && x /= y
--         limI = (ceiling $ ((fromIntegral $ length xs) / (fromIntegral 2))) - 1
--
--
--
-- main = hspec $ do
--   describe "Single Character Palindromes II" $ do
--     it "abba" $ do
--       solve "abba" `shouldBe` False
--     it "abbaa" $ do
--       solve "abbaa" `shouldBe` True
--     it "abbx" $ do
--       solve "abbx" `shouldBe` True
--     it "aa" $ do
--       solve "aa" `shouldBe` False
--     it "ab" $ do
--       solve "ab" `shouldBe` True
--     it "abcba" $ do
--       solve "abcba" `shouldBe` True



-- factors n = [i | i <-[1..n-1], n `mod` i == 0]

isPerfect :: Integer -> Bool
isPerfect n = sum [i | i <-[1..n-1], n `mod` i == 0] == n


-- factors :: Integer -> [Integer]
-- factors n = factor : (factors (fromIntegral n) / (fromIntegral factor))
--   where
--     factor = head $ [i | i <-[2..n-1], n `mod` i == 0]


getFactors :: Int -> [Int]
getFactors n = 1 : getF n

getF :: Integral t => t -> [t]
getF 1 = []
getF n =
  case fact of
    [] -> [n]
    _  -> fact ++ getF (n `div` head fact)
  where fact = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n]

primeFactors :: Integral a => a -> [a]
primeFactors n = primeFactors' n 2
  where
    primeFactors' 1 _ = []
    primeFactors' nn f
      | nn `mod` f == 0 = f : primeFactors' (nn `div` f) f
      | otherwise      = primeFactors' nn (f + 1)



-- too slow
factorList :: Int -> [Int]
factorList value = init $ factorsGreaterOrEqual 1
  where
    factorsGreaterOrEqual test
      | test == value = [value]
      | value `mod` test == 0 = test : restOfFactors
      | otherwise = restOfFactors
      where restOfFactors = factorsGreaterOrEqual (test + 1)


-- too slow
isFactorOf :: Integral a => a -> a -> Bool
isFactorOf x n = n `mod` x == 0

createList :: Integral a => a -> a -> [a]
createList n f | f <= n `div` 2 = if f `isFactorOf` n
                                     then f : next
                                     else next
               | otherwise      = []
    where next = createList n (f + 1)

factor :: Integral a => a -> [a]
factor n = createList n 1



-- module Symbols where
--
-- transform :: String -> String
-- transform s = reverse . map f . reverse
--   where f []     = []
--         f (x:xs) = if elem x xs then [] else [x]


-- alexMistakes :: Int -> Int -> Int
-- alexMistakes numberOfKatas timeLimit = res
--
-- timeOnKatas numberOfKatas timeLimit = 6 * timeLimit `div` numberOfKatas
-- timeRemaining timeLimit timeOnKatas = timeLimit - timeOnKatas
-- geometricSum n = 5 * (2 ^ n - 1)
-- res sums = if (null sums) then 0 else snd $ last $ sums
-- sums timeRemaining = takeWhile (\(sum, i) -> sum <= timeRemaining) [(geometricSum i, i) |i <- [1..]]
--
--
--
-- main = hspec $ describe "alexMistakes" $ do
--   it "10 120 `shouldBe` 3" $ do
--     alexMistakes 10 120 `shouldBe` 3
--   it "11 120 `shouldBe` 3" $ do
--     alexMistakes 11 120 `shouldBe` 3
--   it "3  45 `shouldBe` 2" $ do
--     alexMistakes  3  45 `shouldBe` 2
--   it "8 120 `shouldBe` 3" $ do
--     alexMistakes  8 120 `shouldBe` 3
--   it "6  60 `shouldBe` 2" $ do
--     alexMistakes  6  60 `shouldBe` 2
--   it "9 180 `shouldBe` 4" $ do
--     alexMistakes  9 180 `shouldBe` 4

  -- it "returns 0 if there is no time for mistakes" $ property $ \(Positive x) ->
  --   alexMistakes x (x * 60 `div` 10) `shouldBe` 0


-- let2int :: Char -> Int
-- let2int c
--    | isUpper c = (ord c - ord 'A') `mod` 26
--    | isLower c = ord c - ord 'a'
--    | otherwise = ord c
--
-- -- int2let :: Int -> Char
-- int2let c
--   | ord 'A' > c &&  c < ord 'Z' = chr(ord 'A' + (c `mod` 26))
--   | ord 'a' > c &&  c < ord 'z' = chr(ord 'a' + (c `mod` 26))
--   | otherwise                   = chr c
--
--
-- encode :: String -> String -> String
-- encode key msg = [ int2let ((a + b) ) | (a, b) <- zip keyInts msgInts ]
--   where
--     keyInts = cycle (map let2int key)
--     msgInts = map let2int msg



-- let2int :: Char -> Char -> Int
-- let2int initchr c = ord c - ord initchr
--
-- int2let :: Char -> Int -> Char
-- int2let initchr n = chr (ord initchr + n)
--
-- lower2int :: Char -> Int
-- lower2int = let2int 'a'
--
-- upper2int :: Char -> Int
-- upper2int = let2int 'A'
--
-- int2lower :: Int -> Char
-- int2lower = int2let 'a'
--
-- int2upper :: Int -> Char
-- int2upper = int2let 'A'
--
--
-- shiftcase n c int2case case2int =
--     int2case ((case2int c + n) `mod` 26)
--
--
-- shift :: Int -> Char -> Char
-- shift n c
--     | isLower c = shiftcase n c int2lower lower2int
--     | isUpper c = shiftcase n c int2upper upper2int
--     | otherwise = c
--
--
-- ceasar :: Int -> String -> String
-- ceasar n xs = [shift n x | x <- xs]
--
-- unceasar :: Int -> String -> String
-- unceasar n xs = [shift (-n) x | x <- xs]


gcd' :: (Integral a) => [a] -> a
gcd' []     = 1
gcd' [x]    = x
gcd' (x:xs) = gcd x (gcd' xs)


-- x + y + x - y + x * y + x / y = n
-- 2x + xy + x/y = n
-- 2xy + xy^2 + x = ny
-- x(2y + y^2 + 1) = ny
-- x = ny / (y^2 + 2y + 1)

-- type signature
-- fourPiles :: Int -> Int -> [Int]
-- fourPiles n y





replace :: String -> String
replace = fmap (\c -> if c `elem` "aeiouAEIOU" then '!' else c)

-- go [] = []
-- go (x:xs) = (if x `elem` "aeiouAEIOU" then '!' else x) : go xs


-- sumMul(2, 9)   ==> 2 + 4 + 6 + 8 = 20
-- sumMul(3, 13)  ==> 3 + 6 + 9 + 12 = 30
-- sumMul(4, 123) ==> 4 + 8 + 12 + ... = 1860
-- sumMul(4, -7)  ==> "INVALID"



-- sumMul :: Int -> Int -> Maybe Int
-- sumMul n m = n * x * (x + 1) / 2
--   where x = (m `div` n) * n

-- sumMul :: Int -> Int -> Maybe Int
-- sumMul n m =
-- --   traceShow (n, m) $
--   if m < n || n == 0 || m == n
--     then Nothing
--     else Just $ n * x * (x + 1) `div` 2
--   where x = m `div` n

testOf :: Integer
testOf = ((9223372036854775807 :: Integer) * 2) `div` 2

testOff :: Int
testOff = fromIntegral (((9223372036854775807 :: Integer) * 2) `div` 2)


sumMul :: Int -> Int -> Maybe Int
sumMul n m =
  if m < n || n == 0 || m == n
    then Nothing
    else Just (fromIntegral (toInteger n * toInteger x * (toInteger x + 1) `div` 2) :: Int)
  where x = m `div` n
