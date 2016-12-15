{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import Data.List
import Data.Map as Map (toList, fromListWith)
   
sortTupleList :: [(Char, Integer)] -> [(Char, Integer)]
sortTupleList lst = sortBy tupleOrder lst where

        tupleOrder :: (Char, Integer) -> (Char, Integer) -> Ordering
        tupleOrder (c1, x1) (c2, x2) 
        -- char compared by ord and a is less than b!
          | x1 == x2 && c1 <= c2 = GT 
          | x1 == x2 && c1 >= c2 = LT
          | x1 < x2 = LT 
          | x1 > x2 = GT
          | otherwise = EQ

listMapFromString :: String -> [(Char, Integer)]
listMapFromString xs = toList . fromListWith (+) $ [(c, 1) | c <- xs]

main :: IO ()
main = do
    input <- readFile "in6.txt"
    let xxs = (Prelude.map listMapFromString) . transpose . lines $ input 
    
    let ans = f' $ map (reverse . sortTupleList) xxs where
        f' = foldr f ""
        f (x:_) ac = fst (x):ac
        f [] _ = []
    print $ ans
   