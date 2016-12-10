{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

toTriple :: [Int] -> Triple
toTriple [x, y, z] = (x, y, z)
toTriple _ = (0,0,0)

-- for part 2
countColTriangles :: Triple -> Triple -> Triple -> Int
countColTriangles  (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = n where
 n = length $  filter (==True) [isTriangle (x1, x2, x3),
                                isTriangle (y1, y2, y3),
                                isTriangle (z1, z2, z3)]

-- a window of three that slides along three  at a time ends three from end
countAllColTriangles :: [Triple] -> Int
countAllColTriangles [] = 0
countAllColTriangles (a:b:c:[]) = countColTriangles a b c
countAllColTriangles (a:b:c:xs) = 
    (countColTriangles a b c) + (countAllColTriangles xs)
-- 

toIntList :: [String] -> [Int]
toIntList s = map (\x -> read x :: Int) s

type Triple = (Int, Int, Int)
isTriangle :: Triple -> Bool    
isTriangle (x, y, z) = x + y > z && y + z > x && z + x > y


main :: IO ()
main = do
    messLine <- readFile "in3.txt"
    let list = map toTriple $ map toIntList $ map words (lines messLine) 
    -- part one answer
    let x =  length  [ t | t <- list, isTriangle t]
    -- part two answer
    print $ countAllColTriangles list


   