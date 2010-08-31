import Control.Monad (mapM_)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import System.IO
import Text.Printf

import qualified Data.ByteString.Lazy.Char8 as B

type Name = B.ByteString
type Result = (Name, WinOrLose)

data Stat = Stat Name Int Int

instance Show Stat where
    show (Stat name m o) = printf "%s %d %d" (B.unpack name) m o

data WinOrLose 
    = Win  
    | Lose  
    deriving (Eq, Show)

analyzeLine :: B.ByteString -> Maybe (Name, WinOrLose)
analyzeLine s | (B.pack "WIN") `B.isPrefixOf` s    = Just (B.words s !! 6, Win)
analyzeLine s | (B.pack "DEF") `B.isPrefixOf` s    = Just (B.words s !! 6, Lose)
analyzeLine _                                      = Nothing

analyzeLines :: B.ByteString -> [(Name, WinOrLose)]
analyzeLines = catMaybes . map analyzeLine . B.lines

printAllStats [] = putStrLn ""
printAllStats (x:xs) = do
    print x 
    printAllStats xs


printStat :: [(Name, WinOrLose)] -> IO ()
printStat ws = do
    putStrLn $ show $ length ws
    let results     = map (flip resultsAgainst ws) (unique ws)
    printAllStats results
    where   unique  :: [Result] -> [Name]
            unique    = Set.toList . Set.fromList . Map.keys . Map.fromList 

    
resultsAgainst :: Name -> [Result] -> Stat
resultsAgainst name [] = Stat name 0 0 
resultsAgainst name rs = Stat name cntWin cntLose
    where   cntWin  = length $ filter (\(n, wl) -> n == name && wl == Win) rs
            cntLose = length $ filter (\(n, wl) -> n == name && wl == Lose) rs


main = do
    cont <- B.getContents 
    let ls = analyzeLines cont
    printStat ls

