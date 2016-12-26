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
import Data.Set (Set)
import qualified Data.Set as S

type By = Int 
type Row = Int 
type Col = Int 


type Pixels = Set (Int, Int)

data Screen = Screen
  { maxX :: Int
  , maxY :: Int
  , pixels :: Pixels
  }
instance Show Screen where
  show s = unlines $ map showRow [0 .. (maxY s - 1)]
    where
      showRow :: Row -> String
      showRow y =
        [ if (x, y) `S.member` pixels s then '#' else ' ' | x <- [0 .. (maxX s - 1)] ]

initialScreen :: Screen
initialScreen = Screen 50 6 S.empty

updateScreen :: Screen -> (Pixels -> Pixels) -> Screen
updateScreen s f = s { pixels = f (pixels s) }

data Expr    = Rect Col Row | RotRow Row By | RotCol  Col By | NOP deriving (Show)
type Program = [Expr]
-- ================================= 
-- helper
spacedToken :: String -> Parser String
spacedToken tok = do
    spaces
    t <- string tok
    spaces
    return t

-- rect AxB
parseRect :: Parser Expr
parseRect = do
    
    spacedToken "rect" 
    col <- read <$> many1 digit
    char 'x'
    row <- read <$> many1 digit
    return $ Rect row  col

-- rotate row y=A by B 
parseRotRow :: Parser Expr
parseRotRow = do
   
    spacedToken "rotate"
    spacedToken "row"
    string "y="
    row <- read <$> many1 digit
    spacedToken "by"
    by <- read <$> many1 digit

    return $ RotRow row by

-- rotate column x=0 by 1
parseRotCol :: Parser Expr
parseRotCol = do

    spacedToken "rotate"
    spacedToken "column"
    string "x="
    col <- read <$> many1 digit
    spacedToken "by"
    by <- read <$> many1 digit

    return $ RotCol col by

parseExpr :: Parser Expr 
parseExpr = do
    -- need try as the the text of each expr has common values
    expr <- try parseRect <|> try parseRotRow <|> try parseRotCol    
    return expr
-- =================================

evalExpr :: Screen -> Expr -> Screen
evalExpr s e =

    case e of
        (Rect   r c ) -> evalRect   c r  s
        (RotRow r by) -> evalRotRow r by s
        (RotCol c by) -> evalRotCol c by s
        (NOP        ) -> id s


evalRect :: Col -> Row -> Screen -> Screen
evalRect c r s = updateScreen s (S.union newPix) where
    newPix =
      S.fromList [ (x, y) | x <- [0 .. (c - 1)], y <- [0 .. (r - 1)] ]

evalRotRow :: Row -> By -> Screen -> Screen
evalRotRow r by s = updateScreen s (S.map rotateRow)
  where
    rotateRow (x, y) = if y == r 
        then ((x + by) `mod` maxX s, y) 
        else (x, y)

evalRotCol :: Col -> By -> Screen -> Screen
evalRotCol c by s = updateScreen s (S.map rotateCol)
  where
    rotateCol (x, y) = if x == c
        then (x, (y + by) `mod` maxY s)
        else (x, y)

evalProgram :: Screen -> Program ->  Screen
evalProgram  = foldl evalExpr

pixelsLit :: Screen -> Int
pixelsLit s = length $ S.toList (pixels s)

main :: IO ()
main = do

    input <- readFile "in8.txt" 

    
    case sequence . map (parse parseExpr "") . lines $ input of
        Right exprs -> do
            let scr = evalProgram initialScreen exprs
            print scr
            print $ pixelsLit scr
        Left  err   -> print err
    