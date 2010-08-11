module Game.Logic where

data PlayerChoice = 
    Roll 
    | Save 
    deriving (Eq, Show)

type Move = (PlayerChoice, Int)
type Moves = [Move]

type LogicCallback = ([Moves] -> [Moves] -> PlayerChoice)

stupidLogic :: LogicCallback
stupidLogic own other = Roll

breakAfterPoints :: LogicCallback
breakAfterPoints own other =
    let curMv = currentMoves own
        points = pointsOfMoves curMv
    in 
        case points >= 9 of
            True -> Save
            _ -> Roll

maxPoints :: Int
maxPoints = 50

notLegal :: Int -> Bool
notLegal = (==6)

legal :: Int -> Bool
legal = (<6)

onlyWithLegalPoints :: Moves -> Moves
onlyWithLegalPoints [] = []
onlyWithLegalPoints mvs = filter (\t -> legal $ snd t) mvs
    

sumOfPoints :: [Moves] -> Int
sumOfPoints [] = 0
sumOfPoints mvs = 
    let throws = [y | l <- mvs, (x, y) <- l]
        countingThrows = filter legal throws
    in sum countingThrows


currentMoves :: [Moves] -> Moves
currentMoves [] = []
currentMoves mvs = last mvs


pointsOfMoves :: Moves -> Int
pointsOfMoves [] = 0
pointsOfMoves mvs = 
    let leg = onlyWithLegalPoints mvs
    in sumOfPoints [leg]

