module Game.Logic where

import Game.Gameplay

type Move = Int
type Moves = [Move]

class LogicFunc where
    calculateNextMove :: Moves -> Moves -> PlayerChoice
