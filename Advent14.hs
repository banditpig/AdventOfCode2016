
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
-- import Data.Digest.Pure.MD5
-- import Data.ByteString.Lazy.Char8 
import Control.Monad
single :: String -> Maybe (Char, String)
single [] = Nothing
single (a:xs) = Just (a, xs)

matches :: (Char, String) -> Maybe (Char, String)
matches (_, []) = Nothing
matches (c, (x:xs)) = if c == x then Just (c, xs) else Nothing
    


dbl  :: String -> Maybe (Char, String)
dbl []     = Nothing
dbl [_]    = Nothing

dbl (a:b:[]) = if a == b then Just (a, []) else Nothing
dbl (a:b:xs) 
    | a == b = Just (a, xs)
    | otherwise = dbl (b:xs)

nPl :: Int -> [Char] -> Maybe (Char, String)
nPl n s | length s < n = Nothing
nPl n s  = applyn (n - 3) (dbl s >>= matches) where 
        applyn :: Int -> Maybe (Char, String) -> Maybe (Char, String)
        applyn n' m' = m' >>= foldr (<=<) return (replicate n' matches)
          

main = print "" -- print $ md5 "Hello, world!"