module Game.Statistics where

import Game.Logic

notLegal = (==6)
legal = (<6)

sumOfPoints :: [Moves] -> Int
sumOfPoints [] = 0
sumOfPoints mvs = 
    let throws = [y | l <- mvs, (x, y) <- l]
        countingThrows = filter legal throws
    in sum countingThrows

