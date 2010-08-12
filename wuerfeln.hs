import System.IO
import System.Exit
import Network.Socket

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

getNextMsg :: Socket -> IO ServerMessage
getNextMsg srv = do 
    msg <- readNextLineFromServer srv
    return $ parseServerMessage msg

sendMyChoiceToServer :: Socket -> PlayerChoice -> String -> IO ()
sendMyChoiceToServer srv (Roll) msg = do 
    sendLineToServer srv $ show $ ROLL msg
sendMyChoiceToServer srv (Save) msg = do
    sendLineToServer srv $ show $ SAVE msg
   
checkForEndOfGame :: ServerMessage -> Bool
checkForEndOfGame (WIN _ _ _)   = True
checkForEndOfGame (DEF _ _ _)   = True
checkForEndOfGame _             = False 

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

not' :: WhosInTurn -> WhosInTurn
not' Me         = OtherGuy
not' OtherGuy   = Me


whoDoesTheNextPointsCountFor :: PlayerChoice -> WhosInTurn
whoDoesTheNextPointsCountFor Roll = Me
whoDoesTheNextPointsCountFor Save = OtherGuy

append6 :: [Moves] -> [Moves]
append6 mvs =
    let updated = appendToVeryLastElement 6 mvs
    in updated ++ [[]]

appendEmptyListIfLastElementIsNotEmpty :: [Moves] -> [Moves]
appendEmptyListIfLastElementIsNotEmpty mvs = 
    let l = last mvs
    in
        if null l
            then
                mvs
            else
                mvs ++ [[]]

communicationLoop :: LogicCallback -> Socket -> IO ()
communicationLoop logic server = do
    fstMsg <- getNextMsg server
    let whoStarts = detectWhoStarts fstMsg
    gameLoop fstMsg whoStarts [[]] [[]]
    where   gameLoop :: ServerMessage -> WhosInTurn -> [Moves] -> [Moves] -> IO ()
            gameLoop msg throwCountsFor myMoves otherMoves = do
                putMsg msg
                putStrLn "******"
                putStrLn $ show myMoves
                putStrLn $ show otherMoves
                putStrLn "******"
                case msg of
                    (WIN _ _ _)     -> gameEnded msg
                    (DEF _ _ _)     -> gameEnded msg
                    (TURN _ _ _)    -> do
                                    let     myChoice = logic myMoves otherMoves
                                    sendMyChoiceToServer server myChoice ""
                                    putStrLn $ show myChoice
                                    nextMsg <- getNextMsg server
                                    gameLoop nextMsg (whoDoesTheNextPointsCountFor myChoice) (addAEmptyElementIfISave myChoice myMoves) otherMoves
                                    where   addAEmptyElementIfISave :: PlayerChoice -> [Moves] -> [Moves]
                                            addAEmptyElementIfISave (Save) mvs = mvs ++ [[]]
                                            addAEmptyElementIfISave _ mvs = mvs
                                            
                    (THRW 6 _)      -> do
                                    case throwCountsFor of
                                        Me -> do
                                            nextMsg <- getNextMsg server
                                            gameLoop nextMsg (not' Me) (append6 myMoves) otherMoves
                                        OtherGuy -> do
                                            nextMsg <- getNextMsg server
                                            gameLoop nextMsg (not' OtherGuy) myMoves (append6 otherMoves)
                    (THRW p _)      -> do
                                    case throwCountsFor of
                                        Me -> do
                                            nextMsg <- getNextMsg server
                                            gameLoop nextMsg Me (appendToVeryLastElement p myMoves) otherMoves
                                        OtherGuy -> do
                                            nextMsg <- getNextMsg server
                                            gameLoop nextMsg OtherGuy myMoves (appendToVeryLastElement p otherMoves)
                    _               -> do
                                    putStrLn $ "Unerwartete Nachricht: " ++ (show  msg)
                                    nextMsg <- getNextMsg server
                                    gameLoop nextMsg  throwCountsFor myMoves otherMoves
                                    
mainLoop :: LogicCallback -> Socket -> IO ()
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

