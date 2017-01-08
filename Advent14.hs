
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
<<<<<<< HEAD
import Data.List (nub, tails, null)
import Data.Hash.MD5 (md5s, Str(..))

salt = "ihaygndm"
-- salt = "abc"



part1 :: IO ()
part1 = print $ head $ drop 63 $ filter (\i -> maybeKey sq i && verifyKey sq i) [0..]
    where sq = md5List

part2 :: IO ()
part2 = print $ head $ drop 63 $ filter (\i -> maybeKey sq i && verifyKey sq i) [0..]
    where sq = md5ListStretched

md5List :: [String]
md5List = [makeMd5 i | i <- [0..]]
    where makeMd5 i = md5s (Str (salt ++ show i))

md5ListStretched :: [String]
md5ListStretched = [makeMd5 i | i <- [0..]]
    where makeMd5 i = stretch $ md5s (Str (salt ++ show i)) 
          stretch h0 = foldr (\_ h -> md5s (Str h)) h0 [1..2016]

maybeKey :: [String] -> Int-> Bool
maybeKey s = not . null . repeatN 3 . ((!!) s)

verifyKey :: [String] -> Int -> Bool
verifyKey s i = any (confirmation) $ take 1000 $ drop (i+1) s
    where c = head $ repeatN 3 $ s!!i
          confirmation m = c `elem` (repeatN 5 m)

repeatN :: Int -> String -> [String]
repeatN n = filter (null . tail) . map (nub) . subStr n

subStr :: Int -> [a] -> [[a]]
subStr l = filter (\s -> (length s) == l) . map (take l) . tails


main :: IO ()
main = do 
        
