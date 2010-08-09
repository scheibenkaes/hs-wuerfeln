module Game.Gameplay where

type Message = String

data PlayerChoice = 
    ROLL Message
    | SAVE Message
    deriving (Eq, Show)
