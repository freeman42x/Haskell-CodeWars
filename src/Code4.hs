module Code4 where

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
