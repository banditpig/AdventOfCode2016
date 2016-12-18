{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

type Point = (Int, Int)
data Direction = N | S | W | E deriving (Eq, Show)
type Location = (Direction, Point) 
data Instruction = L Int | R Int | NOP deriving (Show)
type Instructions = [Instruction] 

updateDirection :: Instruction -> Direction -> Direction
updateDirection (L _) d
    | d == N = W
    | d == S = E
    | d == W = S
    | d == E = N
updateDirection (R _) d
    | d == N = E
    | d == S = W
    | d == W = N
    | d == E = S
updateDirection _ d = d

locEq :: Location -> Location -> Bool
locEq (_, (x1, y1)) (_, (x2, y2)) = x1 == x2 && y1 == y2

updateXYCoords :: Int -> Direction -> Location -> [Location]
updateXYCoords n N (d, (x, y) ) = [(d, (x,  y + y')) | y' <- [1..n]]
updateXYCoords n S (d, (x, y) ) = [(d, (x , y - y')) | y' <- [1..n]]
updateXYCoords n W (d, (x, y) ) = [(d, (x - x', y )) | x' <- [1..n]]
updateXYCoords n E (d, (x, y) )=  [(d, (x + x', y )) | x' <- [1..n]]

dist :: Location -> Location -> Int
dist (_, (x1, y1)) (_, (x2, y2)) = (abs (x1 - abs x2)) + (abs (y1 - abs y2))

parseInstructions :: [String] -> Instructions 
parseInstructions  = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction [] = NOP
parseInstruction (x:xs)
    | x == 'L' =  (L (read xs :: Int))
    | x == 'R' =  (R (read xs :: Int))
    | otherwise = NOP


evalInstruction :: Location -> Instruction  -> [Location]
evalInstruction (d, (x, y)) instr  = updateXYCoords n d' (d', (x, y) )
  where 
    d' = updateDirection instr d
    n  = getN instr
    getN (L z) = z
    getN (R z) = z
    getN _ = 0

evalInstructions :: Location -> Instructions -> [Location]
evalInstructions _ [] = []
evalInstructions st (x:xs) = p' ++ (evalInstructions st' xs) where
    p'  = evalInstruction st x 
    st' = last p' -- last is not safe on empty list 



firstDuplicate :: (a -> a -> Bool) -> [a] -> Maybe a
firstDuplicate _ [] = Nothing
firstDuplicate f (x:xs) 
    | matches f x xs = Just x
    | otherwise = firstDuplicate f xs
    where 
        matches _ _ [] = False
        matches f x (x':xs') 
            | f x x' = True
            | otherwise = matches f x xs'


main :: IO ()
main = 
    do
        let inpstr =  ["R1", "R1", "R3", "R1", "R1", "L2", "R5", "L2", "R5", "R1", "R4", "L2", "R3", "L3", "R4", "L5", "R4", "R4", "R1", "L5", "L4", "R5", "R3", "L1", "R4", "R3", "L2", "L1", "R3", "L4", "R3", "L2", "R5", "R190", "R3", "R5", "L5", "L1", "R54", "L3", "L4", "L1", "R4", "R1", "R3", "L1", "L1", "R2", "L2", "R2", "R5", "L3", "R4", "R76", "L3", "R4", "R191", "R5", "R5", "L5", "L4", "L5", "L3", "R1", "R3", "R2", "L2", "L2", "L4", "L5", "L4", "R5", "R4", "R4", "R2", "R3", "R4", "L3", "L2", "R5", "R3", "L2", "L1", "R2", "L3", "R2", "L1", "L1", "R1", "L3", "R5", "L5", "L1", "L2", "R5", "R3", "L3", "R3", "R5", "R2", "R5", "R5", "L5", "L5", "R2", "L3", "L5", "L2", "L1", "R2", "R2", "L2", "R2", "L3", "L2", "R3", "L5", "R4", "L4", "L5", "R3", "L4", "R1", "R3", "R2", "R4", "L2", "L3", "R2", "L5", "R5", "R4", "L2", "R4", "L1", "L3", "L1", "L3", "R1", "R2", "R1", "L5", "R5", "R3", "L3", "L3", "L2", "R4", "R2", "L5", "L1", "L1", "L5", "L4", "L1", "L1", "R1"]
        let instructions = parseInstructions inpstr
        let allPoints = evalInstructions (N, (0,0)) instructions

 
        print $ dist  (N, (0,0)) $ last allPoints
        print $ dist (N, (0,0)) $  maybe (N, (0,0)) id (firstDuplicate locEq allPoints)
-- Input data
-- ["R1", "R1", "R3", "R1", "R1", "L2", "R5", "L2", "R5", "R1", "R4", "L2", "R3", "L3", "R4", "L5", "R4", "R4", "R1", "L5", "L4", "R5", "R3", "L1", "R4", "R3", "L2", "L1", "R3", "L4", "R3", "L2", "R5", "R190", "R3", "R5", "L5", "L1", "R54", "L3", "L4", "L1", "R4", "R1", "R3", "L1", "L1", "R2", "L2", "R2", "R5", "L3", "R4", "R76", "L3", "R4", "R191", "R5", "R5", "L5", "L4", "L5", "L3", "R1", "R3", "R2", "L2", "L2", "L4", "L5", "L4", "R5", "R4", "R4", "R2", "R3", "R4", "L3", "L2", "R5", "R3", "L2", "L1", "R2", "L3", "R2", "L1", "L1", "R1", "L3", "R5", "L5", "L1", "L2", "R5", "R3", "L3", "R3", "R5", "R2", "R5", "R5", "L5", "L5", "R2", "L3", "L5", "L2", "L1", "R2", "R2", "L2", "R2", "L3", "L2", "R3", "L5", "R4", "L4", "L5", "R3", "L4", "R1", "R3", "R2", "R4", "L2", "L3", "R2", "L5", "R5", "R4", "L2", "R4", "L1", "L3", "L1", "L3", "R1", "R2", "R1", "L5", "R5", "R3", "L3", "L3", "L2", "R4", "R2", "L5", "L1", "L1", "L5", "L4", "L1", "L1", "R1"]
-- output 241 and 116