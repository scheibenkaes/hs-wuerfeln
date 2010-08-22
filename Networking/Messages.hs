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
module Networking.Messages where

import Data.List

type Version = String
type Message = String
type MyPoints = Int
type OtherPoints = Int
type Points = Int
type Name = String

data ServerMessage = 
    HELO Version Message
    | DENY Message
    | TURN MyPoints OtherPoints Message
    | THRW Points Message
    | DEF MyPoints OtherPoints Message
    | WIN MyPoints OtherPoints Message
    | Unknown Message
    deriving (Eq)

instance Read ServerMessage where
    readsPrec _ value
        | "HELO" `isPrefixOf` value = readHelo $ words value
        | "DENY" `isPrefixOf` value = readDeny $ words value
        | "TURN" `isPrefixOf` value = read3PtMessage (TURN) (words value)
        | "THRW" `isPrefixOf` value = readThrw $ words value
        | "DEF" `isPrefixOf` value = read3PtMessage (DEF) (words value)
        | "WIN" `isPrefixOf` value = read3PtMessage (WIN) (words value)
        | otherwise = [(Unknown value, "")]
        where   readHelo xs = 
                    [(HELO (xs !! 1) (unwords $ drop 2 xs), "")]
                readDeny xs = 
                    [(DENY $ unwords $ drop 1 xs, "")]
                readThrw xs =
                    [(THRW (read $ xs !! 1 :: Int) (unwords $ drop 2 xs), "")]
                read3PtMessage t xs =
                    let
                    rest = unwords $ drop 3 xs
                    in [(t (read $ xs !! 1 :: Int) (read $ xs !! 2 :: Int) rest, "")]
                    

instance Show ServerMessage where
-- TODO: funktion unwords nutzen
    show (HELO v m)         = "HELO " ++ v ++ " " ++ m
    show (DENY m)           = "DENY " ++ m
    show (TURN my other m)  = "TURN " ++ show my ++ " " ++ show other ++ " " ++ m 
    show (THRW p m)         = "THRW " ++ show p ++ " " ++ m
    show (DEF my other m)   = "DEF " ++ show my ++ " " ++ show other ++ " " ++ m
    show (WIN my other m)   = "WIN " ++ show my ++ " " ++ show other ++ " " ++ m
    show (Unknown m)        = m

data ClientMessage = 
    AUTH Name Message
    | ROLL Message
    | SAVE Message

instance Show ClientMessage where
    show (AUTH n m) = "AUTH " ++ n ++ " " ++ m
    show (ROLL m) = "ROLL " ++ m
    show (SAVE m) = "SAVE " ++ m

parseServerMessage :: String -> ServerMessage
parseServerMessage msg = read msg :: ServerMessage
