module Network.Messages where

type Version = String
type Message = String
type MyPoints = Int
type OtherPoints = Int
type Points = Int
type Name = String

data ServerMessage = HELO Version Message
    | DENY Message
    | TURN MyPoints OtherPoints Message
    | THRW Points Message
    | DEF MyPoints OtherPoints Message
    | WIN MyPoints OtherPoints Message
    deriving (Show, Eq)

data ClientMessage = AUTH Name Message
    | ROLL Message
    | SAVE Message
    deriving (Show, Eq)

