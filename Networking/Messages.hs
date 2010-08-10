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
    deriving (Eq)

instance Read ServerMessage where
    readsPrec _ value
        | "HELO" `isPrefixOf` value = readHelo $ words value
        | "DENY" `isPrefixOf` value = readDeny value
        | otherwise = []
        where   readHelo xs = 
                    [(HELO (xs !! 1) (unwords $ drop 2 xs), "")]
                readDeny s = 
                    let
                    xs = words s
                    in [(DENY $ unwords $ drop 1 xs, "")]

instance Show ServerMessage where
-- TODO: funktion unwords nutzen
    show (HELO v m) = "HELO " ++ v ++ " " ++ m
    show (DENY m)   = "DENY " ++ m
    show (TURN my other m) = "TURN " ++ show my ++ " " ++ show other ++ " " ++ m 
    show (THRW p m) = "THRW " ++ show p ++ " " ++ m
    show (DEF my other m) = "DEF " ++ show my ++ " " ++ show other ++ " " ++ m
    show (WIN my other m) = "WIN " ++ show my ++ " " ++ show other ++ " " ++ m

data ClientMessage = 
    AUTH Name Message
    | ROLL Message
    | SAVE Message

instance Show ClientMessage where
    show (AUTH n m) = "AUTH " ++ n ++ " " ++ m
    show (ROLL m) = "ROLL " ++ m
    show (SAVE m) = "SAVE " ++ m

{- Nicht benoetigt -> Nur Schreiben!
instance Read ClientMessage where
    readsPrec _ value 
        | "AUTH" `isPrefixOf` value = readAuth value
        | "ROLL" `isPrefixOf` value = readRoll value
        | "SAVE" `isPrefixOf` value = readSave value
        | otherwise = []
        where readAuth s = [(AUTH $ )]
        where readRoll s = [(ROLL $ drop 5 value, "")]
                where parts = words s
-}
