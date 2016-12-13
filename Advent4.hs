import Data.Char 
--import Data.Monoid
import Data.Ord
import Data.List.Utils
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as Map (toList, fromListWith)
-- ["aczupnetwp-dnlgpyrpc-sfye-dstaatyr-561[patyc]","jsehsyafy-vqw-ljsafafy-866[nymla]","tyepcyletzylw-ncjzrpytn-prr-opawzjxpye-743[cnrdl]","foadouwbu-qvcqczohs-obozmgwg-662[lamjh]","ckgvutofkj-pkrrehkgt-zkinturume-436[krtue]","pelbtravp-pnaql-ernpdhvfvgvba-481[szram]","yflexwxoalrp-ciltbo-tlohpelm-887[bmwep]"]
-- Each room consists of an encrypted name (lowercase letters separated by dashes) 
-- followed by a dash, a sector ID, and a checksum in square brackets.

-- A room is real (not a decoy) if the checksum is the
-- five most common letters in the encrypted name, in order, with ties
-- broken by alphabetization. 
type CodedRoom     = String
type DecodedName   = String
type SectorID      = String 
type Checksum      = String
type AllCodedRooms = [CodedRoom]
type DecodedRoom   = (DecodedName, SectorID, Checksum)
 
decode :: CodedRoom -> DecodedRoom
decode cr = room where
   
   (decodeName' , r) = (replace "-" "" $ takeWhile (not . isDigit) cr , dropWhile (not . isDigit) cr)
   (sectorId, checkSum) = (takeWhile isDigit r, replace "]" "" $ replace "[" "" $ dropWhile isDigit r)
   tupleList = sortTupleList $ toList $ fromListWith (+) [(c, 1) | c <- decodeName']
   decodeName = (take 5) $ foldl (\ac x  -> ac ++ [(fst x)]) "" tupleList
   room = (decodeName, sectorId, checkSum)

secId :: DecodedRoom -> Int
secId (decodedName, secId, checksum) =(read secId)::Int

splitOnPred ::(a -> Bool) -> [a] -> ([a], [a])
splitOnPred f as = (takeWhile f as, dropWhile f as)

sortTupleList :: [(Char, Int)] -> [(Char, Int)]
sortTupleList lst = reverse $ sortBy tupleOrder lst where

        tupleOrder :: (Char, Int) -> (Char, Int) -> Ordering
        tupleOrder (c1, x1) (c2, x2) 
        -- char compared by ord and a is less than b!
          | x1 == x2 && c1 <= c2 = GT 
          | x1 == x2 && c1 >= c2 = LT
          | x1 < x2 = LT 
          | x1 > x2 = GT

roomReal :: DecodedRoom -> Bool
roomReal (decodedName, secId, checksum) = decodedName == checksum

addIds :: DecodedRoom -> Int -> Int
addIds (decodedName, secId, checksum) acc = acc + (read secId)::Int

evaluate :: AllCodedRooms -> Int
evaluate crs = foldr addIds 0 $ filter roomReal (map decode crs)

main :: IO ()
main = do
  
  inpt <- readFile "in4.txt"
  print $ evaluate (lines inpt)
