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
    | Dunno
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

sendMyChoiceToServer :: Handle -> PlayerChoice -> String -> IO ()
sendMyChoiceToServer srv (Roll) msg = sendLineToServer srv $ show $ ROLL msg
sendMyChoiceToServer srv (Save) msg = sendLineToServer srv $ show $ SAVE msg
   
checkForEndOfGame :: ServerMessage -> IO ()
checkForEndOfGame (WIN my other msg) = (putStrLn "SIEG!! ") >> exitSuccess
checkForEndOfGame (DEF my other msg) = (putStrLn "Mist!") >> exitFailure
checkForEndOfGame _ = return () 


communicationLoop :: LogicCallback -> Handle -> IO ()
communicationLoop logic server = do
    fstMsg <- getNextMsg server
    communicationLoop' fstMsg Dunno [] [(initOtherTurns fstMsg)]
    where   communicationLoop' :: ServerMessage -> WhosInTurn -> [Moves] -> [Moves] -> IO ()
            communicationLoop' lastMsg whoWasLastInTurn myMoves otherMoves = do
                putMsg lastMsg
                checkForEndOfGame lastMsg
                let whosTurnIsItNow = checkWohIsInTurn whoWasLastInTurn lastMsg 
                case whosTurnIsItNow of
                    Me  -> do
                        let myChoice = logic myMoves otherMoves
                            myUpdatedMoves = updateMoves lastMsg myMoves 
                        sendMyChoiceToServer server myChoice "Jeeeehhaaww"
                        continueWithNextMessage whosTurnIsItNow myUpdatedMoves otherMoves
                    OtherGuy -> do
                        let updatedMoves = updateMoves lastMsg otherMoves
                        continueWithNextMessage whosTurnIsItNow myMoves updatedMoves

                where   checkWohIsInTurn :: WhosInTurn -> ServerMessage -> WhosInTurn
                        checkWohIsInTurn (_) (TURN _ _ _)           = Me
                        checkWohIsInTurn (OtherGuy) (THRW 6 _)      = Me
                        checkWohIsInTurn (OtherGuy) (THRW _ _)      = OtherGuy
                        checkWohIsInTurn (Me) (THRW 6 _)            = OtherGuy
                        checkWohIsInTurn (Me) (THRW _ _)            = Me
                        checkWohIsInTurn (Dunno) m                  = detectWhoStarts m

                        updateMoves :: ServerMessage -> [Moves] -> [Moves]
                        updateMoves (THRW p _) [] = [[(Roll, p)]]
                        updateMoves (THRW p _) ms =  
                            let l = last ms
                                i = init ms
                            in i ++ [(l ++ [(Roll, p)])]

                        continueWithNextMessage inTurn my other = do
                            nextMsg <- getNextMsg server
                            communicationLoop' nextMsg inTurn my other
                            
                
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



