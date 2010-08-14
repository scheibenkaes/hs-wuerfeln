{-
    Copyright 2010 Benjamin Klüglein

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Statistics where

import System.IO

import Game.Logic


{-countOccurenceOf :: Int -> Moves -> Int
countOccurenceOf _ []   = 0
countOccurenceOf p mvs  = length $ [y | (x, y) <- mvs, y == p]
-}

countOccurenceOf :: Integer -> RoundResult -> Int
countOccurenceOf _ []   = 0
countOccurenceOf p rr   = length $ [x | x <- rr, x == p]
    

countOccurenceOfInCompleteGame :: Integer -> GameResult -> Int
countOccurenceOfInCompleteGame _ []   = 0
countOccurenceOfInCompleteGame _ [[]] = 0
countOccurenceOfInCompleteGame p mvs =
    let flat = concat mvs
    in countOccurenceOf p flat


putStatisticsOfPlayer :: String -> GameResult -> IO ()
putStatisticsOfPlayer who mvs = do
    putStrLn "****************************************"
    putStrLn $ "Ergebnisse für: " ++ who
    putStrLn $ "Gesamtpunkte:\t" ++ (show $ sumOfAllCountingRounds mvs)
    putStrLn "****************************************"
    putStrLn $ "Die 1:\t\t\t" ++ show (countOccurenceOfInCompleteGame 1 mvs) ++ "x"
    putStrLn $ "Die 2:\t\t\t" ++ show (countOccurenceOfInCompleteGame 2 mvs) ++ "x"
    putStrLn $ "Die 3:\t\t\t" ++ show (countOccurenceOfInCompleteGame 3 mvs) ++ "x"
    putStrLn $ "Die 4:\t\t\t" ++ show (countOccurenceOfInCompleteGame 4 mvs) ++ "x"
    putStrLn $ "Die 5:\t\t\t" ++ show (countOccurenceOfInCompleteGame 5 mvs) ++ "x"
    putStrLn $ "Die 6:\t\t\t" ++ show (countOccurenceOfInCompleteGame 6 mvs) ++ "x"


