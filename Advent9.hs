-- If parentheses or other characters appear within the data referenced by a marker, 
-- that's okay - treat it like normal data, not a marker, and then resume looking
-- for markers after the decompressed section.

-- For example:

--     ADVENT contains no markers and decompresses to itself with no changes, 
--     resulting in a decompressed length of 6.

--     A(1x5)BC repeats only the B a total of 5 times,
--     becoming ABBBBBC for a decompressed length of 7.

--     (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.

--     A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG 
--     for a decompressed length of 11.

--     (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, 
--     but because it's within a data section of another marker, it is not treated any differently from
--     the A that comes after it. It has a decompressed length of 6.

--     X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18),
--     because the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped and not processed further.

-- What is the decompressed length of the file (your puzzle input)? Don't count whitespace.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import System.IO
import Text.Parsec
import Text.Parsec.String


type Len = Int
type Rep = Int
type Remainder = String
type ALine = String
type Compress = (Len, Rep) 
type Fragment = (Int, Compress, Remainder)

evalFragment :: Fragment -> Int
evalFragment (n, c, r) = n + evalCompress c

evalCompress :: Compress -> Int
evalCompress (l, r) = l * r

parseFragment :: Parser Fragment
parseFragment = do
    -- up to (
    pre <- many (noneOf "(")
    char '('
    len <- fmap read (many1 digit)
    char 'x'
    rep <- fmap read (many1 digit)
    char ')'
    remain <- many anyChar
    return $ (length pre, (len, rep), truncRemainer len remain)

truncRemainer :: Int -> Remainder -> Remainder
truncRemainer n r = r' where (_, r') = splitAt n r

fragmentLine :: ALine -> [Fragment]
fragmentLine lin = do
    let parseFrag = parse parseFragment  "" $ filter (/= ' ') lin
    case parseFrag of 
        Left msg -> [(length lin, (0, 0), "")]
        Right frag@(i, comp, remain) -> frag : fragmentLine remain  

processFragments :: [Fragment] -> Int
processFragments = sum . (map evalFragment)

processMessage :: [ALine] -> Int
processMessage   = processFragments . concat . (map fragmentLine) 

main = do
    messLine <- readFile "in9.txt"
    print (processMessage $ lines messLine)

