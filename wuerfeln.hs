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
import System.Exit

import Config
import Game.Logic
import Game.LogicProxy
import Networking.Messages
import Networking.Server
import Statistics

detectWhoStarts :: ServerMessage -> WhosInTurn
detectWhoStarts (TURN _ _ _) = Me
detectWhoStarts (_) = OtherGuy
    

sendMyChoiceToServer :: ServerConnection -> PlayerChoice -> String -> IO ()
sendMyChoiceToServer srv (Roll) msg = do sendLineToServer srv $ show $ ROLL msg
sendMyChoiceToServer srv (Save) msg = do sendLineToServer srv $ show $ SAVE msg
   
gameEnded :: ServerMessage -> GameResult -> GameResult -> IO ()
gameEnded (WIN _ _ _) own other =
                            putStatisticsOfPlayer "mich" own >> 
                            putStatisticsOfPlayer "den Anderen" other  
gameEnded (DEF _ _ _) own other =
                            putStatisticsOfPlayer "mich" own >> 
                            putStatisticsOfPlayer "den Anderen" other
gameEnded msg@_ _ _  = putStrLn $ show msg


communicationLoop :: ServerMessage -> LogicCallback -> ServerConnection -> IO ()
communicationLoop fstMsg lgic servCon = do
    let whoStarts = detectWhoStarts fstMsg
    gameLoop fstMsg whoStarts [[]] [[]]
    where   gameLoop :: ServerMessage -> WhosInTurn -> GameResult -> GameResult -> IO ()
            gameLoop msg throwCountsFor myMoves otherMoves = do
                putMsg msg
                case msg of
                    (WIN _ _ _)     -> gameEnded msg myMoves otherMoves
                    (DEF _ _ _)     -> gameEnded msg myMoves otherMoves
                    (TURN _ _ _)    -> do
                                    myChoice <- lgic myMoves otherMoves
                                    sendMyChoiceToServer servCon myChoice ""
                                    nextMsg <- getNextMsg servCon
                                    gameLoop nextMsg (whoDoesTheNextPointsCountFor myChoice) (addAEmptyElementIfISave myChoice myMoves) (appendAEmptyList otherMoves)
                                    where   addAEmptyElementIfISave :: PlayerChoice -> GameResult -> GameResult
                                            addAEmptyElementIfISave (Save) mvs  = appendAEmptyList mvs
                                            addAEmptyElementIfISave _ mvs       = mvs
                                            
                    (THRW 6 _)      -> do
                                    nextMsg <- getNextMsg servCon
                                    case throwCountsFor of
                                        Me -> do
                                            gameLoop nextMsg OtherGuy (append6 myMoves) otherMoves
                                        OtherGuy -> do
                                            gameLoop nextMsg Me myMoves (append6 otherMoves)
                    (THRW p _)      -> do
                                    nextMsg <- getNextMsg servCon
                                    case throwCountsFor of
                                        Me -> do
                                            gameLoop nextMsg Me (appendToVeryLastElement p myMoves) otherMoves
                                        OtherGuy -> do
                                            gameLoop nextMsg OtherGuy myMoves (appendToVeryLastElement p otherMoves)
                    _               -> do
                                    putStrLn $ "Unerwartete Nachricht: " ++ (show  msg)
                                    nextMsg <- getNextMsg servCon
                                    gameLoop nextMsg  throwCountsFor myMoves otherMoves
                                    
mainLoop :: String -> LogicCallback -> ServerConnection -> IO ()
mainLoop myName lgic servCon = do
    putStrLn "Warte auf HELO"
    helo <- getNextMsg servCon
    putMsg helo
    case helo of
        (HELO _ _)  -> do -- ... Erfolg .. AUTH senden
            putStrLn "Melde an..."
            sendAuth servCon myName
            firstMsg <- getNextMsg servCon
            case firstMsg of
                (DENY m)    -> putStrLn m >> exitFailure
                _           -> do -- Mögen die Spiele beginnen
                    communicationLoop firstMsg lgic servCon
                    disconnectFromServer servCon
        (_)    -> putStrLn "Server sollte eigentlich ein HELO schicken ..." >> exitFailure

main :: IO () 
main = do
    cfg <- getConfig
    conn <- connectToServer (server cfg) (port cfg)
    mainLoop (name cfg) (getLogic (logic cfg)) conn

