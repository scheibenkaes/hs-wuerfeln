import System.IO
import System.Exit

import Networking.Server
import Networking.Messages
import Game.Logic

appName = "hs-wuerfeln"

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

sendMyChoiceToServer :: Handle -> PlayerChoice -> IO ()
sendMyChoiceToServer srv choice = sendLineToServer srv $ show choice 
    

communicationLoop :: LogicCallback -> Handle -> IO ()
communicationLoop logic server = do
    fstMsg <- getNextMsg server
    putMsg fstMsg
    let starter = detectWhoStarts fstMsg
    putStrLn $ show starter
    communicationLoop' fstMsg starter [] [(initOtherTurns fstMsg)]
    where   communicationLoop' :: ServerMessage -> WhosInTurn -> [Moves] -> [Moves] -> IO ()
            communicationLoop' lastMsg whoWasLastInTurn myMoves otherMoves = do
                nextMsg <- getNextMsg server                
                putStrLn $ show nextMsg
                let whosTurnIsItNow = checkWohIsInTurn whoWasLastInTurn nextMsg 
                case whosTurnIsItNow of
                    Me  -> do
                        let myChoice = logic myMoves otherMoves
                            myUpdatedMoves = updateMyMoves nextMsg myMoves 
                        sendMyChoiceToServer server myChoice 
                        communicationLoop' nextMsg Me myUpdatedMoves otherMoves 
                        where
                            updateMyMoves :: ServerMessage -> [Moves] -> [Moves]
                            updateMyMoves (THRW p _) [] = [[(Roll, p)]]
                            updateMyMoves (THRW p _) ms = 
                                let 
                                l = last ms
                                i = init ms
                                in i ++ [(l ++ [(Roll, p)])]
                    _   -> 
                        putStrLn "asd"
                where   checkWohIsInTurn :: WhosInTurn -> ServerMessage -> WhosInTurn
                        checkWohIsInTurn (OtherGuy) (TURN _ _ _) = Me
                        checkWohIsInTurn (OtherGuy) (THRW _ _)   = OtherGuy
                        checkWohIsInTurn (Me) (THRW _ _)         = Me
                
            initOtherTurns :: ServerMessage -> Moves
            initOtherTurns (TURN _ _ _) = []
            initOtherTurns (THRW p _) = [(Roll, p)]
                

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



