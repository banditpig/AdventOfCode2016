-- Filesysem Size  Used  Avail  Use%
-- x0-y0     85   69    16   81
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

{-# LANGUAGE OverloadedStrings #-}



type Id      = String 
type Size    = Int
type Used    = Int
type Avail   = Int
type Percent = Int
type Node    = (Id, Size, Used, Avail, Percent)

toNode :: [String] -> Node
toNode  [id1, s, u, a, p] = (id1, read s::Int, read u::Int, read a::Int, read p::Int)
toNode _                  = ("x", 0, 0, 0, 0)
 
-- Node A is not empty (its Used is not zero).
-- Nodes A and B are not the same node.
-- The data on node A (its Used) would fit on node B (its Avail).
pairs :: [Node] -> Int  
pairs nodes = n where
    n = length  [  ((id1, s, u, a, p), (id2, s', u', a', p')) |
      (id1, s, u, a, p)      <- nodes,
      (id2, s', u', a', p') <- nodes,
      u /= 0,
      id1 /= id2,
      u <= a'

     ]


main :: IO ()
main = do
    
     messLine <- readFile "in22.txt"
     let nodes = map toNode .  map words . lines $ messLine
     print $ pairs nodes