
-- You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad like this:

-- 1 2 3
-- 4 5 6
-- 7 8 9
-- Suppose your instructions are:

-- ["ULLP", "RRDDDP", "LURDLP","UUUUDP"]
-- You start at "5" and move up (to "2"), left (to "1"),
-- and left (you can't, and stay on "1"), so the first button is 1.
-- Starting from the previous button ("1"), you move right twice 
-- (to "3") and then down three times (stopping at "9" after 
-- two moves and ignoring the third), ending up with 9.
-- Continuing from "9", you move left, up, right, down,
--  and left, ending with 8.
-- Finally, you move up four times (stopping at "2"), 
-- then down once, ending with 5.
-- So, in this example, the bathroom code is 1985.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

-- data Direction = U | D | L | R | P deriving (Eq, Show)
type Direction = Char
type X   = Int 
type Y   = Int
type Key = String
type Sequence = [Direction] 
type Cell = (Point, Key) 

infixl 5 @+
(@+) :: Int -> Int -> Int
(@+) = gridPlus 3
-- x -->
--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
type Point = (X, Y)
badPoints :: [Point]
badPoints = [(1,1),(2,1),(4,1),(5,1),
             (1,2),(5,2),
             (1,4),(5,4),
             (1,5),(2,5),(4,5),(5,5) ]

illegalPoint :: Point -> Bool
illegalPoint p@(x, y) = elem p badPoints || x < 1 || y < 1 || x > 5 || y > 5

gridPlus :: Int -> Int -> Int -> Int
gridPlus b x y 
    | x + y <= b = x + y
    | otherwise = x

infixl 5 @-
(@-) :: Int -> Int -> Int
(@-) = gridMinus 

gridMinus :: Int -> Int -> Int
gridMinus x y 
    | x - y >= 1 = x - y 
    | otherwise = x

keyPress :: Point -> Key
keyPress (x, y) = show ( (y-1) * 3 + x)


--  For part 2
--

keyPress' :: Point -> Key
keyPress' (x, y) = 
  case (x, y) of
    (3, 1) -> "1"
    (2, 2) -> "2"
    (3, 2) -> "3"
    (4, 2) -> "4"

    (1, 3) -> "5"
    (2, 3) -> "6"
    (3, 3) -> "7"
    (4, 3) -> "8"
    (5, 3) -> "9"

    (2, 4) -> "A"
    (3, 4) -> "B"
    (4, 4) -> "C"
    (3, 5) -> "D"
    (x, y) -> show x ++ ", " ++ show y



evalDirection' :: Cell -> Direction -> Cell
evalDirection' oldp@((x, y), k) d  = 
    case d of 
        'U'     -> case illegalPoint (x, y - 1) of
                    True -> oldp
                    False -> newp
                    where newp = ((x, y - 1), k)      

        'D'     -> case illegalPoint (x, y + 1) of
                    True  -> oldp
                    False -> newp
                    where newp = ((x, y + 1), k)

        'L'     -> case illegalPoint (x - 1, y) of
                    True  -> oldp
                    False -> newp
                    where newp = ((x - 1, y), k)

        'R'     -> case illegalPoint (x + 1, y) of
                    True  -> oldp
                    False -> newp
                    where newp = ((x + 1, y), k)

        'P'     -> ((x, y),keyPress' (x, y))
        '_'     -> ((x, y), k)
-- 


evalDirection :: Cell -> Direction -> Cell
evalDirection ((x, y), k) d  = 
    case d of 
        'U'     -> ((x      , y @- 1),            k)
        'D'     -> ((x      , y @+ 1),            k)
        'L'     -> ((x @- 1 , y    ),             k)
        'R'     -> ((x @+ 1 , y     ),            k)
        'P'     -> ((x      , y     ), keyPress (x, y))
        '_'     -> ((x, y), k)

-- one line of directions
-- and fEval is the strategy for eval of direction based on which
-- grid is being used.
evalSeqLine ::(Cell -> Direction -> Cell) -> Sequence -> Cell -> Cell
evalSeqLine fEval s startCell  = cell' where
  cell' =  foldl fEval startCell s 
 
evalAllSeqLines :: (Cell -> Direction -> Cell) -> [Sequence] -> Cell -> [Cell]
evalAllSeqLines fEval (s:ss) cell = cell' : evalAllSeqLines fEval ss cell'  where 
  cell' = (evalSeqLine fEval s cell)

main :: IO ()
main = do
  input <- readFile "in2.txt"
  -- add a 'press' to end of each line
  let allSeqs = map (++"P") $ lines input
  -- -- do part one  
  -- let keyCode =  [ k | ((a, b), k) <- evalAllSeqLines evalDirection allSeqs ((2, 2), "0")]
  -- or use this below for part 2
  let keyCode =  [ k | ((a, b), k) <- evalAllSeqLines evalDirection' allSeqs ((1, 3), "0")]

  print keyCode

  
