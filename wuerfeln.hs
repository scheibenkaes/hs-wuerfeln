
import Networking.Server
import Game.Logic

mainLoop :: LogicCallback -> IO ServerConnection -> IO ()
mainLoop logic server = do
    putStrLn "asd"

main :: IO () 
main = do
    mainLoop stupidLogic $ connectToServer defaultServer defaultPort



