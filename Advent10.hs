{-# LANGUAGE OverloadedStrings #-}

import Data.Set (Set)
import qualified Data.Set as S

data Chip = Chip { chipId :: Int, handler :: Bot} deriving (Show)
type ChipSet = Set Chip  

data Bin  = Bin { binId :: Int, contents :: ChipSet} deriving (Show)

data Bot  = Bot { botId :: Int,
                  chips :: ChipSet,  
                  low   :: Destination, 
                  hi    :: Destination} deriving (Show)

data Destination = BinDest Bin | BotDest Bot | Empty deriving (Show)

type Bins = [Bin]
type Bots = [Bot]


instance Eq Chip where
    (==) c1 c2 = chipId c1 == chipId c2

instance Ord Chip where
   compare c1 c2 = compare (chipId c1) (chipId c2)


updateBinContents :: Chip -> Bin -> Bin
updateBinContents c b =  b { contents = S.insert c $ contents b}

updateChipSet :: Chip -> Bot  -> Bot
updateChipSet c b = b { chips = S.insert c $ chips b }

makeBot :: Int -> Destination -> Destination -> Bot
makeBot i dl dh = Bot i S.empty dl dh

canActivate :: Bot -> Bool
canActivate b = S.size (chips b)  == 2 


-- route :: Destination -> Chip
-- route (BinDest bin) =  undefined
-- activate :: Bot -> Bot
-- activate = undefined




