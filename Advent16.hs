{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

-- Start with an appropriate initial state (your puzzle input). 
-- Then, so long as you don't have enough data yet to fill the disk, repeat the following steps:

-- Call the data you have at this point "a".
-- Make a copy of "a"; call this copy "b".
-- Reverse the order of the characters in "b".
-- In "b", replace all instances of 0 with 1 and all 1s with 0.
-- The resulting data is "a", then a single 0, then "b".
-- For example, after a single step of this process,

-- 1 becomes 100.
-- 0 becomes 001.
-- 11111 becomes 11111000000.
-- 111100001010 becomes 1111000010100101011110000.

-- Repeat these steps until you have enough data to fill the desired disk.

-- Once the data has been generated, you also need to create a checksum of that data.
-- Calculate the checksum only for the data that fits on the disk, even if you 
--     generated more data than that in the previous step.

-- The checksum for some given data is created by considering each non-overlapping 
-- pair of characters in the input data. 
-- If the two characters match (00 or 11), the next checksum character is a 1.
-- If the characters do not match (01 or 10), the next checksum character is a 0. 
-- This should produce a new string which is exactly half as long as the original. 
-- If the length of the checksum is even, repeat the process until you end up with a \
-- checksum with an odd length.

-- replace multiple a based on a functions
replace :: (a -> a)  -> [a] -> [a]
replace  f  = map (\x -> f x) 

f10 :: Char -> Char
f10 '1' = '0'
f10 '0' = '1'
f10  x  = x

replace10 :: String -> String
replace10 = replace f10


morph :: String -> String
morph s = s ++ ['0'] ++ (replace10 . reverse $ s)

morphUntil :: Int -> String -> String
morphUntil 0 s = s
morphUntil n s = 
    if (length m) >= n 
        then m 
    else 
        morphUntil (n-1) m
    where m = morph s

checksum' :: String -> String
checksum' [] = []
checksum' [_] = []
checksum' (a:b:[]) 
    | a == b = "1"
    | otherwise = "0"
checksum' (a:b:xs)
    | a == b = '1' : checksum' xs
    | otherwise = '0' : checksum' xs

checksum :: String -> String
checksum s 
    | isOdd (length cs) = cs
    | otherwise = checksum cs 
    where cs = checksum' s
          isOdd i  = (i `rem` 2 ) /= 0

main :: IO ()
main = do
    print $ checksum . take 272      .  morphUntil 272      $ "01000100010010111"
    print $ checksum . take 35651584 .  morphUntil 35651584 $ "01000100010010111"
