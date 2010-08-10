import System.IO
import System.Exit

import Networking.Server
import Networking.Messages
import Game.Logic

appName = "hs-wuerfeln"

putMsg msg = 
    putStrLn $ show msg 

data WhosTurn = Mine | OtherGuy

--detectWhoStarts :: ServerMessage -> WhosTurn

didSignupSucceed :: ServerMessage -> Bool
didSignupSucceed (HELO _ _) = True
didSignupSucceed (_) = False

checkSignup msg = 
    if didSignupSucceed msg
    then 
        putStrLn "Verbindung hergestellt!"
    else 
        putStrLn "Verbindung verweigert!" >> exitFailure

getNextMsg :: Handle -> IO ServerMessage
getNextMsg srv = do 
    msg <- readNextLineFromServer srv
    return $ parseServerMessage msg

communicationLoop :: LogicCallback -> Handle -> IO ()
communicationLoop logic server = do
    fstMsg <- getNextMsg server
    putStrLn $ show fstMsg

mainLoop :: LogicCallback -> Handle -> IO ()
mainLoop logic server = do
    putStrLn "Melde an..."
    msg <- authenticate server
    putMsg msg
    checkSignup msg
    communicationLoop logic server
    where
        authenticate conn = do
            str <- sendAuth conn appName
            let msg = read str :: ServerMessage
            return msg



main :: IO () 
main = do
    conn <- connectToServer defaultServer defaultPort
    mainLoop stupidLogic conn



