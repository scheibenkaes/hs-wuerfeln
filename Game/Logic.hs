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

data WhosInTurn = 
      Me 
    | OtherGuy
    deriving (Show, Eq)

type ThrowResult    = Int
type RoundResult    = [ThrowResult]
type GameResult     = [RoundResult]

type LogicCallback = (GameResult -> GameResult -> PlayerChoice)

keepRolling :: LogicCallback
keepRolling _ _ = Roll

breakAfterThrows :: LogicCallback
breakAfterThrows own _ | (length $ last own) == 4 = Save
breakAfterThrows _ _                            = Roll
    

breakAfterPoints :: LogicCallback
breakAfterPoints own _ =
    let curMv = currentRound own
        points = pointsOfRound curMv
    in 
        case points >= 15 of
            True -> Save
            _ -> Roll


moderateAggressive :: LogicCallback
moderateAggressive own other =
    let myPoints        = sumOfAllCountingRounds own
        otherPoints     = sumOfAllCountingRounds other
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

inCloseRange :: Int -> Bool
inCloseRange p = p <= 6

maxPoints :: Int
maxPoints = 50

notLegal :: Int -> Bool
notLegal = (==6)

legal :: Int -> Bool
legal = (<6)

isCountingRound :: RoundResult -> Bool
isCountingRound []    = False
isCountingRound rr    =
    let l = last rr
    in legal l

onlyWithLegalPoints :: GameResult -> GameResult
onlyWithLegalPoints [] = []
onlyWithLegalPoints gr = filter isCountingRound gr

sumOfAllCountingRounds :: GameResult -> Int
sumOfAllCountingRounds [] = 0
sumOfAllCountingRounds mvs = 
    let  
        toBeCounted = concat $ filter isCountingRound mvs
    in sum toBeCounted


currentRound :: GameResult -> RoundResult
currentRound [] = []
currentRound mvs = last mvs


pointsOfRound :: RoundResult -> Int
pointsOfRound [] = 0
pointsOfRound mvs = 
    let leg = filter legal mvs
    in sum leg

appendToVeryLastElement :: Int -> GameResult -> GameResult
appendToVeryLastElement n [[]] = [[n]]
appendToVeryLastElement n ms =
        let l = last ms
            i = init ms
        in i ++ [(l ++ [n])]


whoDoesTheNextPointsCountFor :: PlayerChoice -> WhosInTurn
whoDoesTheNextPointsCountFor Roll = Me
whoDoesTheNextPointsCountFor Save = OtherGuy

append6 :: GameResult -> GameResult
append6 mvs =
    let updated = appendToVeryLastElement 6 mvs
    in updated ++ [[]]

appendAEmptyList :: GameResult -> GameResult
appendAEmptyList gr | not $ null $ last gr  = gr ++ [[]]
appendAEmptyList gr                         = gr
