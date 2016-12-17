{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import Text.Parsec
import Text.Parsec.String
import Control.Monad (sequence)

type Outer  = String
type Inner  = String 
type Group  = (Outer, Inner, Outer)
type IPLine = [Group]


groupParser :: Parser Group
groupParser = do
    oleft <- many (noneOf "[")
    char  '['
    i <- many (noneOf "]") 
    char  ']'
    oright <- many (noneOf "[")
    return (oleft, i, oright)

lineParser :: Parser IPLine
lineParser = do
    g <- many (groupParser )
    return g

-- Yes, Prelude does have any and all  but I just wanted to do them :)
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' g (x:xs) 
 | g x = True
 | otherwise = any' g xs

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' g (x:xs)
  | g x = all' g xs
  | otherwise = False

findABA :: [Char] -> [Maybe (Char, Char, Char)]
findABA (a:b:c:[]) 
 | isABA a b c  = [Just (a, b, c)]
 | otherwise    = [Nothing]
findABA (a:b:c:xs) 
 | isABA a b c  = Just (a, b, c) : findABA (b:c:xs)
 | otherwise    = Nothing : findABA (b:c:xs)
findABA _ = [Nothing]

isABA :: Char -> Char -> Char -> Bool
isABA a b c = a == c && b /= c 


hasBAB ::(Char, Char, Char) -> String -> Bool
hasBAB (a, b, a') (aa':bb':cc':[])     = aa' == b && cc' == b && bb' == a && a == a'
hasBAB tup@(a, b, a') (aa':bb':cc':xs) = aa' == b && cc' == b && bb' == a && a == a' || hasBAB tup (bb':cc':xs) 
hasBAB _ _ = False

isAbba :: String -> Bool
isAbba (a:b:c:d:xs) =  a == d && b == c && a /= b || isAbba (b:c:d:xs) 
isAbba [] = False
isAbba _  = False 

innerOKTLS :: Group -> Bool
innerOKTLS (_, innr, _) = not . isAbba $ innr

outersOKTLS :: Group -> Bool
outersOKTLS (lo, _, ro) = isAbba lo || isAbba ro

ipLineIsTLS :: IPLine -> Bool
ipLineIsTLS ipLn  = all' innerOKTLS ipLn && any' outersOKTLS ipLn

countTLS :: [IPLine] -> Int 
countTLS  = length . filter ( (==)True ) . map ipLineIsTLS 
   
checkIt :: IPLine ->  Maybe (Char, Char, Char) -> Bool
checkIt _ Nothing = False 
checkIt ipLn (Just (a, b, c)) = any' (\(_, innr, _) -> (hasBAB (a, b, c) innr)  ) ipLn


ipLineIsSSL :: IPLine -> Bool
ipLineIsSSL ipLn  = foldr (\x ac -> ac || checkIt ipLn x) False (concat maybeABAs)
  where
    maybeABAs = foldr (\(l, _, r) ac -> findABA r : findABA l : ac) [] ipLn   

countSSL :: [IPLine] -> Int
countSSL = length . filter ( (==)True ) . map ipLineIsSSL 

-- =============
main :: IO ()
main = do
    input <- readFile "in7.txt"
    let parsedLines = sequence . map (parse lineParser "") . lines $ input
    case parsedLines of 
        Right pLines -> do
            print "TLS Count"
            print $ countTLS pLines 
            print "SSL Count"
            print $ countSSL pLines 
        _            -> print "err"
    


