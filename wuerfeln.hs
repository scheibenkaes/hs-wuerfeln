import System.IO
import System.Exit

import Networking.Server
import Networking.Messages
import Game.Logic

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

getNextMsg :: Handle -> IO ServerMessage
getNextMsg srv = do 
    msg <- readNextLineFromServer srv
    return $ parseServerMessage msg

sendMyChoiceToServer :: Handle -> PlayerChoice -> String -> IO ()
sendMyChoiceToServer srv (Roll) msg = sendLineToServer srv $ show $ ROLL msg
sendMyChoiceToServer srv (Save) msg = sendLineToServer srv $ show $ SAVE msg
   
checkForEndOfGame :: ServerMessage -> Bool
checkForEndOfGame (WIN _ _ _)   = True
checkForEndOfGame (DEF _ _ _)   = True
checkForEndOfGame _             = False 

gameEnded :: ServerMessage -> IO ()
gameEnded w@(WIN _ _ _) = ( putStrLn $ show w ) >> exitSuccess
gameEnded w@(DEF _ _ _) = ( putStrLn $ show w ) >> exitFailure
gameEnded msg@_ = putStrLn $ show msg

-- ms darf nicht leer sein!!!
appendToVeryLastElement :: Int -> [Moves] -> [Moves]
appendToVeryLastElement n ms =
        let l = last ms
            i = init ms
        in i ++ [(l ++ [(Roll, n)])]

updateMoves :: ServerMessage -> [Moves] -> [Moves]
updateMoves (THRW p _) [] = [[(Roll, p)]]
updateMoves (THRW 6 _) ms = 
    let l = appendToVeryLastElement 6 ms
    in l ++ []
updateMoves (THRW p@_ _) ms = appendToVeryLastElement p ms 
updateMoves _ ms = ms


updateMovesForActivePlayer :: ServerMessage -> WhosInTurn -> [Moves] -> [Moves] -> ([Moves],  [Moves])
updateMovesForActivePlayer msg (Me) mine other = (updateMoves msg mine, other)
updateMovesForActivePlayer msg (OtherGuy) mine other = (mine, updateMoves msg other)

not' :: WhosInTurn -> WhosInTurn
not' Me         = OtherGuy
not' OtherGuy   = Me

communicationLoop :: LogicCallback -> Handle -> IO ()
communicationLoop logic server = do
    fstMsg <- getNextMsg server
    let whoStarts = detectWhoStarts fstMsg
    communicationLoop' fstMsg whoStarts [] [(initOtherTurns fstMsg)]
    where   communicationLoop' :: ServerMessage -> WhosInTurn -> [Moves] -> [Moves] -> IO ()
            communicationLoop' lastMsg whoIsInTurn myMoves otherMoves = do
                putMsg lastMsg
                let newVals = updateMovesForActivePlayer lastMsg whoIsInTurn myMoves otherMoves
                let myUpdatedMoves = fst newVals
                let otherUpdatedMoves = snd newVals
                if checkForEndOfGame lastMsg
                    then 
                        gameEnded lastMsg 
                    else
                        let continue = (continueWithNextMessage myUpdatedMoves otherUpdatedMoves)
                        in case (lastMsg, whoIsInTurn) of
                            ((TURN _ _ _), _) -> do
                                sendNextMoveToServer
                                continue Me 
                            ((THRW 6 _), Me) -> do
                                continue $ not' Me
                            ((THRW _ _), Me) -> do
                                sendNextMoveToServer
                                continue Me 
                            ((THRW _ _), OtherGuy) -> do
                                continue OtherGuy
                                
                        where   sendNextMoveToServer :: IO ()
                                sendNextMoveToServer = do
                                    let myChoice = logic myMoves otherMoves
                                    sendMyChoiceToServer server myChoice "Jeeeehhaaww"
                                
                                continueWithNextMessage my other inTurn = do
                                    hPutStrLn stderr $ show my
                                    hPutStrLn stderr $ show other
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

