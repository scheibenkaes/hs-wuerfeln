import System.IO

import Networking.Server
import Networking.Messages
import Game.Logic

appName = "hs-wuerfeln"

mainLoop :: LogicCallback -> Handle -> IO ()
mainLoop logic server = do
    putStrLn "Melde an..."
    --sendAuth server appName
    success <- authenticate server
    putStrLn $ show success
    putStrLn "Verbindung hergestellt!"
    where
        authenticate conn = do
            str <- sendAuth conn appName
            let msg = read str :: ServerMessage
            return msg



main :: IO () 
main = do
    conn <- connectToServer defaultServer defaultPort
    mainLoop stupidLogic conn



