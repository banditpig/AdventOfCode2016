{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 hiding (map, length, take, drop, elem)
import Text.Read

type Pos = Int
fiveZerosChar :: [Char] -> Maybe (Pos, Char)
fiveZerosChar ('0':'0':'0':'0':'0':pos:c:_) = Just (digitToInt pos, c)
fiveZerosChar _ = Nothing

md5Hash :: String -> String
md5Hash  = show . md5 . pack  

allDone :: String -> Bool
allDone  = not . elem '*' 

findACharAndLoc :: Int -> String -> (Pos, Char, Int )
findACharAndLoc ix text = 
    case fiveZerosChar md of
        Nothing -> findACharAndLoc (ix + 1) text
        Just (p, c) -> if p <= 7 then (p, c, ix) else findACharAndLoc (ix + 1) text
        where md = md5Hash (text ++ show ix)


fillLocWithChar :: Char -> Int -> String -> String
fillLocWithChar c loc target 
  | target !! loc /= '*' = target -- already been done
  | otherwise = take loc target ++ [c] ++ drop (loc + 1) target


findPassword' :: Bool -> Int -> String -> String -> String
findPassword' True _ partial _ = partial
findPassword' _ ix partial txt = findPassword' isDone (ix' + 1) partial' txt where
    (pos, ch, ix') = findACharAndLoc ix txt
    partial' = fillLocWithChar ch pos partial
    isDone = allDone partial'

findPassword :: String -> String
findPassword txt = findPassword' False 0 "********" txt

-- Just run main
-- main = print $ findPassword "reyedfim"

