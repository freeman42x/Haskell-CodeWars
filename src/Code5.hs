module Code5 where

import           Data.List
import           Data.List.Split
import           Text.Printf



stat :: String -> String
stat strg
  | null strg = ""
  | otherwise = "Range: " ++ range ++
               " Average: " ++ average ++
               " Median: " ++ median
  where
    inSeconds = fmap hmsToSeconds $ splitOn ", " strg
    [range, average, median] = fmap secondsToHms
      [maximum inSeconds - minimum inSeconds,
      (sum inSeconds) `div` (length inSeconds),
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
