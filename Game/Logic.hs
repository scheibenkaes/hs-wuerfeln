module Game.Logic where

import Game.Gameplay

type Move = (PlayerChoice, Int)
type Moves = [Move]

type LogicCallback = ([Moves] -> [Moves] -> PlayerChoice)

stupidLogic :: LogicCallback
stupidLogic own other = Roll
