module Game.Gameplay where

type Message = String

data PlayerChoices = 
    ROLL Message
    | SAVE Message
