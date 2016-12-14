{-# LANGUAGE OverloadedStrings #-}

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 hiding (map, length)

fiveZerosChar :: [Char] -> Maybe Char
fiveZerosChar ('0':'0':'0':'0':'0':x:_) = Just x
fiveZerosChar (_) = Nothing

md5Hash :: String -> String
md5Hash  = show . md5 . pack  

findPwdChar :: Int -> String -> (Char, Int)    
findPwdChar startIndex str =
    case fiveZerosChar $ md5Hash (str ++ (show startIndex)) of
        Nothing -> findPwdChar (startIndex + 1) str
        Just c -> (c, startIndex)

findPassword' :: Int -> Int -> String -> String
findPassword' 0 _ _ = ""
findPassword' cnt ix txt = ch : findPassword' (cnt -1) newIx txt where 
    (ch, newIx)  = findPwdChar (ix + 1) txt

findPassword text = findPassword' (length text) 0 text
main = print $ md5 "Hello, world!"

