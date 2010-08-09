module Main () where

import Networking.Server
import Game.Logic

mainLoop :: LogicCallback -> ServerConnection -> IO ()
mainLoop logic server = putStrLn "ASD"

main :: IO () 
main = do
    mainLoop stupidLogic $ connectToServer "localhost" port



