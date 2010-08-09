module Communication where

import Networking.Messages
import Game.Gameplay

convertChoiceToMessage :: PlayerChoice -> String -> String
convertChoiceToMessage CALL s = "ROLL " ++ s ++ "\n"
convertChoiceToMessage FOLD s = "SAVE " ++ s ++ "\n"

