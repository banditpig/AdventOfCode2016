
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
type Key = Int
type Sequence = [Direction] 
type Cell = (X, Y, Key) 

infixl 5 @+
(@+) :: Int -> Int -> Int
(@+) = gridPlus 3

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

keyPress :: Int -> Int -> Key
keyPress x y = ( (y-1) * 3) + (x)

evalDirection :: Cell -> Direction -> Cell
evalDirection (x, y, k) d  = 
    case d of 
        'U'     -> (x      , y @- 1,            k)
        'D'     -> (x      , y @+ 1,            k)
        'L'     -> (x @- 1 , y     ,            k)
        'R'     -> (x @+ 1 , y     ,            k)
        'P'     -> (x      , y     , keyPress x y)
        '_'     -> (x, y, k)

-- one line of directions
evalSeqLine :: Sequence -> Cell -> Cell
evalSeqLine s startCell  = cell' where
   cell' =  foldl (\cell d -> evalDirection cell d ) startCell s 
 
evalAllSeqLines :: [Sequence] -> Cell -> [Cell]
evalAllSeqLines (s:ss) cell = cell' : evalAllSeqLines ss cell'  where 
  cell' = (evalSeqLine s cell)

main :: IO ()
main = do
  input <- readFile "in2.txt"
  -- add a 'press' to end of each line
  let allSeqs = map (++"P") $ lines input 
  let keyCode =  [ k | (a, b, k) <- evalAllSeqLines allSeqs (2, 2, 0)]
  print keyCode
  
