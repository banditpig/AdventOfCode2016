
-- You arrive at the first floor of this new building to discover a much less welcoming environment 
-- than the shiny atrium of the last one. 
-- Instead, you are in a maze of twisty little cubicles, all alike.

-- Every location in this area is addressed by a pair of non-negative integers (x,y).
-- Each such coordinate is either a wall or an open space. You can't move diagonally. 
-- The cube maze starts at 0,0 and seems to extend infinitely toward positive x and y;
--  negative values are invalid, as they represent a location outside the building.
--  You are in a small waiting area at 1,1.

-- While it seems chaotic, a nearby morale-boosting poster explains, 
-- the layout is actually quite logical. 
-- You can determine whether a given x,y coordinate will be a wall or an open space 
-- using a simple system:

-- Find x*x + 3*x + 2*x*y + y + y*y.
-- Add the office designer's favorite number (your puzzle input).
-- Find the binary representation of that sum; count the number of bits that are 1.
-- If the number of bits that are 1 is even, it's an open space.
-- If the number of bits that are 1 is odd, it's a wall.
-- For example, if the office designer's favorite number were 10, 
-- drawing walls as # and open spaces as ., the corner of the building containing 0,0 
-- would look like this:
--   0123456789
-- 0 .#.####.##
-- 1 ..#..#...#
-- 2 #....##...
-- 3 ###.#.###.
-- 4 .##..#..#.
-- 5 ..##....#.
-- 6 #...##.###
-- Now, suppose you wanted to reach 7,4. The shortest route you could take is marked as O:
    
--   0123456789
-- 0 .#.####.##
-- 1 .O#..#...#
-- 2 #OOO.##...
-- 3 ###O#.###.
-- 4 .##OO#OO#.
-- 5 ..##OOO.#.
-- 6 #...##.###
-- Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current location, 1,1).
-- What is the fewest number of steps required for you to reach 31,39?
-- Your puzzle input is 1362.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import Numeric ( showIntAtBase)
import Data.Char (intToDigit)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

puzzleInput :: Int
puzzleInput = 10

type Cell = (Int, Int) 
type Cells = Set Cell

data Map = Map

  { 
    cells :: Cells
  }

showMap :: Int -> Int -> String
showMap w h = unlines $ map showOneRow [0 .. h]
    where 
      showOneRow y =
        [ if isOpen (x, y)  then '#' else '.' | x <- [0 .. w] ]

endPoint :: Cell 
endPoint = (7, 4)

type Path = [Cell]
type Solutions = [Path]


neighbours :: Cell -> [Cell]
neighbours (x, y) = filter isOpen neighbours' where 
    neighbours' 
        | x == 0 && y == 0 = [(x + 1, y), (x, y + 1)              ]
        | x == 0           = [(x + 1, y), (x, y + 1) , (x, y - 1) ]
        | y == 0           = [(x + 1, y), (x - 1, y ), (x, y + 1) ]
        | otherwise        = [(x + 1, y), (x - 1, y ), (x, y + 1),(x, y - 1) ]

f' :: Int ->  Cell  -> Int
f' fav  (x, y) = fav +  x * x + 3 * x + 2 * x * y + y + y * y 
f :: Cell -> Int
f = f' puzzleInput

toBinStr :: Int -> String 
toBinStr x = showIntAtBase 2 intToDigit x "" 

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

isOpen :: Cell -> Bool
isOpen  (x, y)  = isEven . length . filter (=='1') .  toBinStr $  f (x, y)

main :: IO ()
main = do
    print $ neighbours (7, 1)

 


