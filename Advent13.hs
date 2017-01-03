
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
-- 5 ..##OOO .#.
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
import qualified Control.Monad.State as St
import Data.Map ((!))
import qualified Data.Map as M


puzzleInput :: Int
puzzleInput = 1362

endPoint :: Cell 
endPoint = (31, 39)

startPoint :: Cell
startPoint = (1, 1)

type Cell = (Int, Int) 

data MapState = MapState {
    start      :: Cell,
    target     :: Cell,
    visitedSet :: Set Cell,
    queue      :: [Cell],
    dist       :: M.Map Cell Int
    } deriving (Show)

initialMapState :: Cell -> Cell -> MapState
initialMapState startCell targetCell = MapState startCell targetCell startSet q d  where 
     
    startSet =  S.empty
    q = neighbours startCell
    d = M.fromList  $ map (\x -> (x, 1)) (neighbours startCell)


neighbours :: Cell -> [Cell]
neighbours (x, y) =
  [ neighbr | neighbr@(x', y') <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
              , x' >= 0 && y' >= 0
              , isOpen neighbr ]

isChild :: Cell -> Cell -> Bool
isChild child parent = child `elem` neighbours parent

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



relax :: St.State MapState MapState
relax = do
    st <- St.get
    -- remove first in q and add it to visited set
    let (hdQ:newQ) = queue st
        nbs = neighbours hdQ
        parentDistance = (dist st) ! hdQ
        dist' = M.fromList  $ map (\x -> (x, parentDistance + 1)) nbs  
        -- all neighbours are parentDistance + 1 a
        -- put all neighbours hdQ in map with value of parentDistance + 1
    
    St.put $ st { visitedSet = S.insert hdQ (visitedSet st), 
                       queue = nub $ newQ ++ nbs,
                        dist = M.union (dist st) dist'
                         } 
        
    return $ st  


evalMap :: St.State MapState MapState
evalMap = do
    st <- St.get
    case S.member (target st)  (visitedSet st) of
        True  -> return $ st
        False -> do
            relax
            evalMap 


main :: IO () 
main = do

    let st = St.evalState  evalMap (initialMapState startPoint endPoint)  
    --  part one
    print $ (!) (dist st) endPoint 
    -- part two
    print $ length . M.filter ( <= 50) $ (dist st)


