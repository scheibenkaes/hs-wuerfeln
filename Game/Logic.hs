{-
    Copyright 2010 Benjamin Klüglein

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Game.Logic where

data PlayerChoice = 
    Roll 
    | Save 
    deriving (Eq, Show)

type ThrowResult    = Integer
type RoundResult    = [ThrowResult]
type GameResult     = [RoundResult]

type LogicCallback = (GameResult -> GameResult -> PlayerChoice)

keepRolling :: LogicCallback
keepRolling own other = Roll

breakAfterPoints :: LogicCallback
breakAfterPoints own other =
    let curMv = currentMoves own
        points = pointsOfMoves curMv
    in 
        case points >= 10 of
            True -> Save
            _ -> Roll

moderateAggressive :: LogicCallback
moderateAggressive own other =
    let myPoints        = sumOfPoints own
        otherPoints     = sumOfPoints other
        myPointsLeft    = maxPoints - myPoints
        otherPointsLeft = maxPoints - otherPoints
        amIInRange      = inCloseRange myPointsLeft
        otherInRange    = inCloseRange otherPointsLeft
    in  
        if amIInRange || otherInRange
            then
                keepRolling own other
            else
                breakAfterPoints own other

inCloseRange :: Integer -> Bool
inCloseRange p = p <= 6

maxPoints :: Integer
maxPoints = 50

notLegal :: Integer -> Bool
notLegal = (==6)

legal :: Integer -> Bool
legal = (<6)

onlyWithLegalPoints :: Moves -> Moves
onlyWithLegalPoints [] = []
onlyWithLegalPoints mvs = filter (\t -> legal $ snd t) mvs
    

sumOfPoints :: [Moves] -> Integer
sumOfPoints [] = 0
sumOfPoints mvs = 
    let  
        toBeCounted = filter doesRoundCount mvs
        throws = [p | m <- toBeCounted, (x, p) <- m]
        countingThrows = filter legal throws
    in sum countingThrows


currentMoves :: [Moves] -> Moves
currentMoves [] = []
currentMoves mvs = last mvs


pointsOfMoves :: Moves -> Integer
pointsOfMoves [] = 0
pointsOfMoves mvs = 
    let leg = onlyWithLegalPoints mvs
    in sumOfPoints [leg]

doesRoundCount :: Moves -> Bool
doesRoundCount []   = False
doesRoundCount mvs  =
    let (mv,p) = last mvs
    in legal p
