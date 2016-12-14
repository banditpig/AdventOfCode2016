{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 hiding (map, length, take, drop, elem)

type Pos = Char
fiveZerosChar :: [Char] -> Maybe (Pos, Char)
fiveZerosChar ('0':'0':'0':'0':'0':pos:c:_) = Just (pos, c)
fiveZerosChar _ = Nothing

md5Hash :: String -> String
md5Hash  = show . md5 . pack  

allDone :: String -> Bool
allDone  = not . elem '*' 

findACharLoc :: Int -> String -> (Pos, Char)
findACharLoc ix text = 
    case fiveZerosChar md of
        Nothing -> findACharLoc (ix + 1) text
        Just (p, c) -> (p, c)
        where md = md5Hash (text ++ show ix)


findPassword :: String -> String
findPassword = undefined

main = print $ md5 "Hello, world!"

