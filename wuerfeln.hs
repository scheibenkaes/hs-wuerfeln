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
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Game.Logic
import Game.LogicProxy
import Networking.Messages
import Networking.Server
import Statistics

appName :: String
appName = "olddice"


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

data Flag = 
      Logic     String
    | Server    String
    | Port      String
    deriving Show

options :: [OptDescr Flag]
options = 
    [ 
        Option ['s'] ["server"] (OptArg serv "SERVER")  "Server address",
        Option ['p'] ["port"]   (OptArg prt "PORT")     "Port number"
    ]
    where   serv, prt :: Maybe String -> Flag
            serv    = Server    . fromMaybe defaultServer
            prt     = Port      . fromMaybe defaultPort

main :: IO () 
main = do
    args <- getArgs
    let (opts, nopts, errs) = getOpt RequireOrder options args
    let logic = getLogic $ fromArgs args
    print $ opts !! 0
    print $ srvAddr $ opts !! 0
    conn <- connectToServer (srvAddr (opts !! 0)) defaultPort
    mainLoop logic conn
    where
        fromArgs :: [String] -> Maybe String
        fromArgs []     = Nothing
        fromArgs xs     = Just $ xs !! 0

        srvAddr :: Flag -> String
        srvAddr (Server s) = s
            

