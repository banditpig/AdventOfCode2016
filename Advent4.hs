import Data.Char 
import Data.List.Utils
import Data.Map as Map (toList, fromListWith)
-- Each room consists of an encrypted name (lowercase letters separated by dashes) 
-- followed by a dash, a sector ID, and a checksum in square brackets.

-- A room is real (not a decoy) if the checksum is the
-- five most common letters in the encrypted name, in order, with ties
-- broken by alphabetization. For example:
-- aaaaa-bbb-z-y-x-123[abxyz]
-- takeWhile (\x -> isLetter x || (x == '-')) $ takeWhile (\x -> isDigit x == False) s
type CodedRoom     = String
type DecodedName   = String
type SectorID      = String 
type Checksum      = String
type AllCodedRooms = [CodedRoom]
type DecodedRoom   = (DecodedName, SectorID, Checksum)



decode :: CodedRoom -> DecodedRoom
decode cr = room where
    -- aaaaa-bbb-z-y-x-123[abxyz]
    -- will be decodeName = aaaaabbbzyx
    -- r = 123[abxyz]
   (decodeName , r) = (replace "-" "" $ takeWhile (not . isDigit) cr , dropWhile (not . isDigit) cr)
   (sectorId, checkSum) = (takeWhile isDigit r, replace "]" "" $ replace "[" "" $ dropWhile isDigit r)
   -- eg [('a',5),('b',3),('x',1),('y',1),('z',1)]
   pairList = fromListWith (+) [(c, 1) | c <- decodeName]
   -- letters = foldr stringFst  "" pairList  where 
   --             stringFst (fs, sec) str = str ++ fs
   room = (decodeName, sectorId, checkSum)


roomReal :: DecodedRoom -> Bool
roomReal = undefined

sumIDs :: DecodedRoom -> Int -> Int
sumIDs = undefined

evaluate :: AllCodedRooms -> Int
evaluate crs = foldr sumIDs 0 $ filter roomReal (map decode crs)


