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
