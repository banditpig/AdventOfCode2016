import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M

data ProgState = ProgState
  { progCount :: Int, 
    regs      :: (Map Char Int) }   deriving (Show)

initialState :: ProgState
initialState = ProgState 0 $ M.fromList [ ('a', 0),('b', 0),('c', 0),('d', 0) ]

type StringState = Int
type StringVal   = Int
-- type StringProc a = State StringState a
-- M.insert "a" (m M.! "a" + 2) m
--        key      funct          old map         modified map
update :: Char -> (Int -> Int) -> Map Char Int -> Map Char Int 
update key f mp = M.insert key newVal  mp where
    newVal = f (mp M.! key)


evalString :: String -> State ProgState ProgState 
evalString [] = do
    st <- get
    return $  st
evalString (x:xs)  = do
    st <- get
    case x of 
        'a' -> put (ProgState  ((progCount st) + 1) (update 'a' (+1) (regs st) )) -- ((progCount st) + 1)
        'b' -> put (ProgState  ((progCount st) + 2) (update 'b' (+1) (regs st) ))
        'c' -> put (ProgState  ((progCount st) + 3) (update 'c' (+1) (regs st) ))
        'd' -> put (ProgState  ((progCount st) + 4) (update 'd' (+1) (regs st) ))
        _   -> put (st)

    evalString xs

main = print $ evalState (evalString "abcde") initialState
