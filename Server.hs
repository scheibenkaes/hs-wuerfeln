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
module Main where

import Control.Concurrent (forkIO)
import Data.Maybe
import Network.Socket
import System.IO

import Networking.Messages
import Server.Connectivity
import Server.ServerLogic

type ClientConnection   = (Socket, SockAddr)

data GameState   = 
      GameReadyToStart          ClientConnection ClientConnection
    | WaitingForAnotherPlayer   ClientConnection
    | WaitingForTwoPlayers      
    deriving (Show)

serverVersion :: String
serverVersion = "0.1"

sayHeloTo :: Handle -> IO ()
sayHeloTo pl = do
    let helo = HELO serverVersion "On your marks"
    sendToClient pl helo

startMatch :: Player -> Player -> IO ()
startMatch player1 player2 = do
    putStrLn "Starting match..."
    starting <- detectWhoStarts player1 player2
    let m = Match {
          firstPlayer   = fst starting
        , secondPlayer  = snd starting
    }
    result <- runMatch m
    finishMatch result

quitConnection :: String -> [Player] -> IO ()
quitConnection _ [] = return ()
quitConnection m (p:ps) = hPutStrLn h m >> hClose h >> quitConnection m ps
    where h = connection p

prepareMatch :: ClientConnection -> ClientConnection -> IO ()
prepareMatch p1 p2 = do
    putStrLn "Start match"
    hP1 <- transformClientConnection $ fst p1
    hP2 <- transformClientConnection $ fst p2

    sayHeloTo hP1 
    sayHeloTo hP2

    player1 <- authenticateClient hP1
    player2 <- authenticateClient hP2

    case all isJust [player1, player2] of
        True    -> startMatch (fromJust player1) (fromJust player2)
        _       -> quitConnection "Not all players did authenticate correctly." [fromJust player1, fromJust player2] 
    
    where   transformClientConnection :: Socket -> IO Handle
            transformClientConnection s = do
                h <- socketToHandle s ReadWriteMode
                hSetBuffering h LineBuffering
                return h

authenticateClient :: Handle -> IO (Maybe Player)
authenticateClient h = do
    msg <- readNextClientMessage h
    case msg of 
        (AUTH n _) ->  return $ Just $ Player { 
                              name = n
                            , connection = h
                            , points = 0
                            }
        _          ->  return Nothing

mainLoop :: Socket -> GameState -> IO ()
mainLoop master gameState = do
    plCon <- accept master
    case handleIncomingConnection plCon gameState of
        (WaitingForTwoPlayers)                  -> mainLoop master WaitingForTwoPlayers
        waiting@(WaitingForAnotherPlayer _)     -> do
            putStrLn "Waiting for a second player."
            mainLoop master waiting
        (GameReadyToStart p1 p2)            -> do
            -- Start match
            _ <- forkIO (prepareMatch p1 p2)
            mainLoop master WaitingForTwoPlayers

    where   handleIncomingConnection :: ClientConnection -> GameState -> GameState
            handleIncomingConnection newConn WaitingForTwoPlayers           = WaitingForAnotherPlayer newConn
            handleIncomingConnection newConn (WaitingForAnotherPlayer first)  = GameReadyToStart first newConn
            handleIncomingConnection _ st    = error $ "Illegal state: " ++ show st
                

main :: IO () 
main = do
    masterSock <- listenTo port
    mainLoop masterSock WaitingForTwoPlayers
    where port = "3333"
