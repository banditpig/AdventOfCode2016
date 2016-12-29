-- The assembunny code you've extracted operates on four registers (a, b, c, and d) 
-- that start at 0 and can hold any integer. However, it seems to make use of only 
-- a few instructions:

-- cpy x y copies x (either an integer or the value of a register) into register y.
-- inc x increases the value of register x by one.
-- dec x decreases the value of register x by one.
-- jnz x y jumps to an instruction y away (positive means forward; negative means backward), 
-- but only if x is not zero.
-- The jnz instruction moves relative to itself: an offset of -1 would continue at 
-- the previous instruction, while an offset of 2 would skip over the next instruction.

-- For example:

-- cpy 41 a
-- inc a
-- inc a
-- dec a
-- jnz a 2
-- dec a
-- The above code would set register a to 41, increase its value by 2, 
-- decrease its value by 1, and then skip the last dec a 
-- (because a is not zero, so the jnz a 2 skips it), leaving register a at 42.
-- When you move past the last instruction, the program halts.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}


import Debug.Trace
import qualified Control.Monad.State as S
import Data.Map (Map, (!))
import qualified Data.Map as M
import Text.Parsec 
import Text.Parsec.String

data Reg   = Reg Char deriving (Show)
data Value = LitValue  Int | RegValue Reg deriving (Show)
data Instr = Inc Reg | Dec Reg | Copy Value Reg | Jnz Value Int  deriving (Show)

data ProgState = ProgState
  { progCount      :: Int,
    program        :: [Instr],
    regs           :: (Map Char Int) }   deriving (Show)



initialState :: [Instr] -> ProgState
initialState instr  = ProgState 0 instr $  M.fromList [ ('a', 0),('b', 0),('c', 0),('d', 0) ]

parseInc :: Parser Instr
parseInc = do
    string "inc "
    reg <- oneOf "abcd"
    return (Inc $ Reg reg)

parseDec :: Parser Instr
parseDec = do
    string "dec "
    reg <- oneOf "abcd"
    return (Dec $ Reg reg)

parseLitteralVal :: Parser Value
parseLitteralVal = do
    n <- read <$> many1 digit
    return $ LitValue n

parseRegValue :: Parser Value
parseRegValue = do
   reg <- oneOf "abcd"  
   return $ RegValue (Reg reg)


parseValue :: Parser Value
parseValue = do
    value <- try parseLitteralVal <|> try parseRegValue
    return value

parseCopy :: Parser Instr
parseCopy = do
    string "cpy "
    val <- parseValue
    spaces
    ch  <- oneOf "abcd"  
    return $ Copy val  (Reg ch)


parseJmp :: Parser Instr
parseJmp = do
    string "jnz "
    val <- parseValue
    spaces
    sign <- option '+' (char '-')
    digits <- many1 digit
    return $ Jnz  val $ if sign == '-' then -1 * read digits else 1  * read digits


parseInstr :: Parser Instr 
parseInstr = do
    instr <- try parseInc <|> try parseDec <|> try parseCopy <|> try parseJmp  
    return instr

--        key      function       old map         modified map
update :: (Int -> Int)  -> Char ->  Map Char Int -> Map Char Int 
update f key  mp = M.insert key newVal  mp where
    newVal = f (mp M.! key)

updateWith :: Int  -> Char ->  Map Char Int -> Map Char Int 
updateWith newVal key  mp = M.insert key newVal  mp 


regValue :: Char -> Map Char Int -> Int
regValue = flip (!)

getValue :: Value -> S.State ProgState Int
getValue (LitValue n) = return n
getValue (RegValue (Reg ch)) = do
    st <- S.get
    return $ regValue ch (regs st)


evalInstr :: Instr -> S.State ProgState ProgState 
evalInstr (Copy val (Reg ch)) = do
    st <- S.get
    v <- getValue val
    let ix = (progCount st)
    S.put $ st { progCount = ix + 1, regs = updateWith v ch $ regs st }
    return st

evalInstr (Inc (Reg ch)) = do
    st <- S.get   
    let ix = (progCount st)
    S.put $ st { progCount = ix + 1, regs = update (+1)  ch $ regs st }
    return st

evalInstr (Dec (Reg ch)) = do
    st <- S.get    
    let ix = (progCount st)
    S.put $ st { progCount = ix + 1, regs = update ( +(-1))  ch $ regs st }
    return st

evalInstr (Jnz val n) = do
    st <- S.get
    let ix = (progCount st)
    v <- getValue val
    -- if v == 0 then ix' = ix + 1 else ix' = ix + v
    case  (v == 0) of -- trace ("jnz v " ++ show v) $
        True  -> S.put $ st { progCount = ix + 1 }
        False -> S.put $ st { progCount = ix + n }
    return st
   


evalProg :: S.State ProgState ProgState 
evalProg = do
    st <- S.get
    let ix = (progCount st)
    

    evalInstr   $ (program st) !! ix -- $ trace ("inst " ++ show inst)
    case ix  >=  ( length . program $ st ) of 
        True  -> return $ st
        False -> evalProg 

    

main :: IO ()
main = do

    input <- readFile "in12.txt" 

    case sequence . map (parse parseInstr "") . lines $ input of
        Right instrs -> do
            print $ S.evalState evalProg $ initialState instrs
            -- for part 2 just change the initialState to have value 1 for 'c'
        Left  err   -> print err
    

