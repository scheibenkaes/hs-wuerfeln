module Game.Logic where

import Game.Gameplay

type Move = (PlayerChoice, Int)
type Moves = [Move]

class LogicFunc a where
    calculateNextMove :: 
        [a] -> -- Eigene Zuege
        [a] -> -- Gegnerische Zuege
        PlayerChoice -- Wuerfeln oder nicht wuerfeln, das ist hier die Frage
