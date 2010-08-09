module Communication where

import Data.List

import Networking.Messages hiding (ROLL, SAVE)
import Game.Gameplay

convertChoiceToMessage :: PlayerChoice -> String -> String
convertChoiceToMessage ROLL s = "ROLL " ++ s ++ "\n"
convertChoiceToMessage SAVE s = "SAVE " ++ s ++ "\n"

convertServerMessageToChoice :: String -> PlayerChoice
convertServerMessageToChoice msg    
    | "ROLL " `isPrefixOf` msg = ROLL
    | "SAVE " `isPrefixOf` msg = SAVE
