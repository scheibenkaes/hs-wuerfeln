module Networking.Messages where

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
    deriving (Eq, Read)

instance Show ServerMessage where
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
    deriving (Show, Eq, Read)

