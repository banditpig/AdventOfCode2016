-- The magnetic strip on the card you swiped encodes a series of instructions 
-- for the screen; these instructions are your puzzle input. 
-- The screen is 50 pixels wide and 6 pixels tall, all of which start off, 
-- and is capable of three somewhat peculiar operations:

-- rect AxB 
-- turns on all of the pixels in a rectangle at the top-left
-- of the screen which is A wide and B tall.

-- rotate row y=A by B 
-- shifts all of the pixels in row A (0 is the top row) right by B pixels. 
-- Pixels that would fall off the right end appear at the left end of the row.

-- rotate column x=A by B 
-- shifts all of the pixels in column A (0 is the left column) down by B pixels.
-- Pixels that would fall off the bottom appear at the top of the column.
-- rect 1x1
-- rotate row y=0 by 10
-- rect 1x1
-- rotate column x=0 by 1
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import Text.Parsec
import Text.Parsec.String

type Row     = Int
type Col     = Int
type By      = Int
data Screen  = S
data Expr    = Rect Row Col | RotRow Row By | RotCol  Col By | NOP -- need NOP?
type Program = [Expr]

-- ================================= 
-- rect AxB
parseRect :: Parser Expr
parseRect = do
    spaces
    string "rect"
    row <- read <$> many1 digit
    char 'x'
    col <- read <$> many1 digit
    return $ Rect row col

-- rotate row y=A by B 
parseRotRow :: Parser Expr
parseRotRow = do
    spaces 
    string "row"
    spaces
    string "y="
    row <- read <$> many1 digit
    spaces
    string "by"
    spaces
    by <- read <$> many1 digit

    return $ RotRow row by

-- rotate column x=0 by 1
parseRotCol :: Parser Expr
parseRotCol = do
    spaces
    string "rotate"
    spaces
    string "column"
    spaces 
    string "x="
    col <- read <$> many1 digit
    spaces
    string "by"
    spaces
    by <- read <$> many1 digit
    return $ RotCol col by
-- =================================

evalExpr :: Expr -> Screen -> Screen
evalExpr e s =
    case e of
        (Rect   r c) -> evalRect   r c s
        (RotRow r c) -> evalRotRow r c s
        (RotCol r c) -> evalRotCol r c s
        (NOP       ) -> id s

evalRect :: Row -> Col -> Screen -> Screen
evalRect = undefined

evalRotRow :: Row -> Col -> Screen -> Screen
evalRotRow = undefined

evalRotCol :: Row -> Col -> Screen -> Screen
evalRotCol = undefined


evalProgram :: Screen -> Program ->  Screen
evalProgram  = foldr evalExpr



main :: IO ()
main = print "Compiles!"
    










