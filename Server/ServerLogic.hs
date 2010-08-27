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
module Server.ServerLogic where

import System.IO
import Text.Printf

import Numeric.Probability.Game.Dice

import Networking.Messages
import Server.Connectivity

data Player = Player {
      name          :: String
    , connection    :: Handle
    , points        :: Int
}

data Match = Match {
      firstPlayer   :: Player
    , secondPlayer  :: Player
}

data Result = Result {
    match :: Match
}

detectWhoStarts :: Player -> Player -> IO (Player, Player)
detectWhoStarts p1 p2 = do
    isEven <- even
    if isEven
        then return (p1, p2)
        else return (p2, p1)
    where   even = do
            val <- roll d6  
            return $ val `mod` 2 == 0

rollDice :: IO Int
rollDice = roll d6

runMatch :: Match -> IO Result
runMatch = runMatch' [] 

sendTurnTo :: Player -> Player -> IO ()
sendTurnTo p1 p2 = do
    let turn = TURN (points p1) (points p2) "Du bist dran."
    sendToClient (connection p1) turn

sendThrwTo :: Player -> Player -> Int -> IO ()
sendThrwTo p1 p2 p = do
    let thrw = THRW p (printf " hat Spieler %s gewürfelt. Weiter gehts" (name p1)) 
    sendToClient (connection p1) thrw 
    sendToClient (connection p2) thrw


sendWin :: Player -> Player -> IO ()
sendWin p otherPlayer = do
    let win = WIN (points p) (points otherPlayer) (printf "Du gewinnst gegen %s" (name otherPlayer))
    sendToClient (connection p) win

sendDef :: Player -> Player -> IO ()
sendDef p otherPlayer = do
    let def = DEF (points p) (points otherPlayer) (printf "Du verlierst gegen %s" (name otherPlayer))
    sendToClient (connection p) def

finishMatch :: Result -> IO ()
finishMatch res = do
    let m = match res
        first = firstPlayer m
        second = secondPlayer m
    sendWin first second
    sendDef second first
    putStrLn $ printf "Endergebnis: \n\t%s %d gg. %s %d" (name first) (points first) (name second) (points second)
    
readNextClientMessage :: Handle -> IO ClientMessage
readNextClientMessage h = do
    line <- hGetLine h
    let msg = parseClientMessage line
    return msg

runMatch' :: [Int] -> Match -> IO Result
runMatch' ps match = do
    let playerWhoIsInTurn   = firstPlayer match 
        otherPlayer         = secondPlayer match

    sendTurnTo (savePoints playerWhoIsInTurn ps) otherPlayer
    choice <- readNextClientMessage (connection playerWhoIsInTurn)
    case choice of
        (ROLL _) -> do
            dice <- rollDice
            sendThrwTo playerWhoIsInTurn otherPlayer dice
            case dice of 
                6   -> runMatch' [] (match { firstPlayer = otherPlayer, secondPlayer = playerWhoIsInTurn })
                p   -> do
                    let sumAfterRoll    = (points playerWhoIsInTurn) + sum ps + p
                        winner          = sumAfterRoll >= 50
                    print sumAfterRoll
                    case winner of
                        True    -> return $ Result (match {firstPlayer = playerWhoIsInTurn {points = sumAfterRoll}, secondPlayer = otherPlayer})
                        _       -> runMatch' (dice:ps) match

        (SAVE _) -> runMatch' [] (match {firstPlayer = otherPlayer, secondPlayer = savePoints playerWhoIsInTurn ps})
        _        -> error "Unknown message!"

    where
        savePoints :: Player -> [Int] -> Player
        savePoints p ps= p {points = (points p) + (sum ps)}

