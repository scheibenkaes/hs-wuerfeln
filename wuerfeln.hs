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
import System.IO

import Config
import Game.Logic
import Game.LogicProxy
import Networking.Messages
import Networking.Server
import Statistics

detectWhoStarts :: ServerMessage -> WhosInTurn
detectWhoStarts (TURN _ _ _) = Me
detectWhoStarts (_) = OtherGuy
    
didSignupSucceed :: ServerMessage -> Bool
didSignupSucceed (HELO _ _) = True
didSignupSucceed (_) = False

checkSignup :: ServerMessage -> IO ()
checkSignup msg = 
    if didSignupSucceed msg
    then 
        putStrLn "Verbindung hergestellt!"
    else 
        putStrLn "Verbindung verweigert!" >> exitFailure


sendMyChoiceToServer :: ServerConnection -> PlayerChoice -> String -> IO ()
sendMyChoiceToServer srv (Roll) msg = do sendLineToServer srv $ show $ ROLL msg
sendMyChoiceToServer srv (Save) msg = do sendLineToServer srv $ show $ SAVE msg
   
gameEnded :: ServerMessage -> GameResult -> GameResult -> IO ()
gameEnded w@(WIN _ _ _) own other =
                            putStatisticsOfPlayer "mich" own >> 
                            putStatisticsOfPlayer "den Anderen" other  
gameEnded w@(DEF _ _ _) own other =
                            putStatisticsOfPlayer "mich" own >> 
                            putStatisticsOfPlayer "den Anderen" other
gameEnded msg@_ _ _  = putStrLn $ show msg


communicationLoop :: LogicCallback -> ServerConnection -> IO ()
communicationLoop logic server = do
    fstMsg <- getNextMsg server
    let whoStarts = detectWhoStarts fstMsg
    gameLoop fstMsg whoStarts [[]] [[]]
    where   gameLoop :: ServerMessage -> WhosInTurn -> GameResult -> GameResult -> IO ()
            gameLoop msg throwCountsFor myMoves otherMoves = do
                putMsg msg
--                hPutStrLn stderr $ show myMoves
--                hPutStrLn stderr $ show otherMoves
                case msg of
                    (WIN _ _ _)     -> gameEnded msg myMoves otherMoves
                    (DEF _ _ _)     -> gameEnded msg myMoves otherMoves
                    (TURN _ _ _)    -> do
                                    let     myChoice = logic myMoves otherMoves
                                    sendMyChoiceToServer server myChoice ""
                                    nextMsg <- getNextMsg server
                                    gameLoop nextMsg (whoDoesTheNextPointsCountFor myChoice) (addAEmptyElementIfISave myChoice myMoves) (appendAEmptyList otherMoves)
                                    where   addAEmptyElementIfISave :: PlayerChoice -> GameResult -> GameResult
                                            addAEmptyElementIfISave (Save) mvs  = appendAEmptyList mvs
                                            addAEmptyElementIfISave _ mvs       = mvs
                                            
                    (THRW 6 _)      -> do
                                    nextMsg <- getNextMsg server
                                    case throwCountsFor of
                                        Me -> do
                                            gameLoop nextMsg OtherGuy (append6 myMoves) otherMoves
                                        OtherGuy -> do
                                            gameLoop nextMsg Me myMoves (append6 otherMoves)
                    (THRW p _)      -> do
                                    nextMsg <- getNextMsg server
                                    case throwCountsFor of
                                        Me -> do
                                            gameLoop nextMsg Me (appendToVeryLastElement p myMoves) otherMoves
                                        OtherGuy -> do
                                            gameLoop nextMsg OtherGuy myMoves (appendToVeryLastElement p otherMoves)
                    _               -> do
                                    putStrLn $ "Unerwartete Nachricht: " ++ (show  msg)
                                    nextMsg <- getNextMsg server
                                    gameLoop nextMsg  throwCountsFor myMoves otherMoves
                                    
mainLoop :: String -> LogicCallback -> ServerConnection -> IO ()
mainLoop name logic server = do
    putStrLn "Melde an..."
    msg <- authenticate 
    putMsg msg
    checkSignup msg
    communicationLoop logic server
    disconnectFromServer server
    where
        authenticate = do
            str <- sendAuth server name
            let msg = read str :: ServerMessage
            return msg

main :: IO () 
main = do
    cfg <- getConfig
    conn <- connectToServer (server cfg) (port cfg)
    mainLoop (name cfg) (getLogic (logic cfg)) conn
    where
        fromArgs :: [String] -> Maybe String
        fromArgs []     = Nothing
        fromArgs xs     = Just $ xs !! 0

        optServ, optPort, optName :: Flag -> String
        optServ (Server s)  = s
        optPort (Port s)    = s
        optName (Name s)    = s

        {-optVal obj (x:xs) std = 
            case x of
                (obj s) = s
                (_)     = optVal obj xs std
        optVal _ [] std = std
-}

