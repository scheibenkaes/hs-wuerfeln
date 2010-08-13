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

import Networking.Server
import Networking.Messages
import Game.Logic
import Game.LogicProxy

appName :: String
appName = "hs-wuerfeln"

putMsg :: ServerMessage -> IO ()
putMsg msg = 
    putStrLn $ show msg 

data WhosInTurn = 
      Me 
    | OtherGuy
    deriving (Show, Eq)

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

getNextMsg :: ServerConnection -> IO ServerMessage
getNextMsg srv = do 
    msg <- readNextLineFromServer srv
    return $ parseServerMessage msg

sendMyChoiceToServer :: ServerConnection -> PlayerChoice -> String -> IO ()
sendMyChoiceToServer srv (Roll) msg = do 
    sendLineToServer srv $ show $ ROLL msg
sendMyChoiceToServer srv (Save) msg = do
    sendLineToServer srv $ show $ SAVE msg
   
gameEnded :: ServerMessage -> IO ()
gameEnded w@(WIN _ _ _) = ( putStrLn $ show w ) >> exitSuccess
gameEnded w@(DEF _ _ _) = ( putStrLn $ show w ) >> exitFailure
gameEnded msg@_ = putStrLn $ show msg

appendToVeryLastElement :: Int -> [Moves] -> [Moves]
appendToVeryLastElement n [] = [[(Roll, n)]]
appendToVeryLastElement n ms =
        let l = last ms
            i = init ms
        in i ++ [(l ++ [(Roll, n)])]


whoDoesTheNextPointsCountFor :: PlayerChoice -> WhosInTurn
whoDoesTheNextPointsCountFor Roll = Me
whoDoesTheNextPointsCountFor Save = OtherGuy

append6 :: [Moves] -> [Moves]
append6 mvs =
    let updated = appendToVeryLastElement 6 mvs
    in updated ++ [[]]


communicationLoop :: LogicCallback -> ServerConnection -> IO ()
communicationLoop logic server = do
    fstMsg <- getNextMsg server
    let whoStarts = detectWhoStarts fstMsg
    gameLoop fstMsg whoStarts [[]] [[]]
    where   gameLoop :: ServerMessage -> WhosInTurn -> [Moves] -> [Moves] -> IO ()
            gameLoop msg throwCountsFor myMoves otherMoves = do
                putMsg msg
                --hPutStrLn stderr $ show myMoves
                --hPutStrLn stderr $ show otherMoves
                case msg of
                    (WIN _ _ _)     -> gameEnded msg
                    (DEF _ _ _)     -> gameEnded msg
                    (TURN _ _ _)    -> do
                                    let     myChoice = logic myMoves otherMoves
                                    sendMyChoiceToServer server myChoice ""
                                    nextMsg <- getNextMsg server
                                    gameLoop nextMsg (whoDoesTheNextPointsCountFor myChoice) (addAEmptyElementIfISave myChoice myMoves) otherMoves
                                    where   addAEmptyElementIfISave :: PlayerChoice -> [Moves] -> [Moves]
                                            addAEmptyElementIfISave (Save) mvs = mvs ++ [[]]
                                            addAEmptyElementIfISave _ mvs = mvs
                                            
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
                                    
mainLoop :: LogicCallback -> ServerConnection -> IO ()
mainLoop logic server = do
    putStrLn "Melde an..."
    msg <- authenticate server
    putMsg msg
    checkSignup msg
    communicationLoop logic server
    disconnectFromServer server
    where
        authenticate conn = do
            str <- sendAuth conn appName
            let msg = read str :: ServerMessage
            return msg

main :: IO () 
main = do
    conn <- connectToServer defaultServer defaultPort
    mainLoop breakAfterPoints conn

