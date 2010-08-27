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
import Network.Socket
import System.IO

import Server.Connectivity

type ClientConnection   = (Socket, SockAddr)

data GameState   = 
      GameReadyToStart          ClientConnection ClientConnection
    | WaitingForAnotherPlayer   ClientConnection
    | WaitingForTwoPlayers      
    deriving (Show)

prepareMatch :: ClientConnection -> ClientConnection -> IO ()
prepareMatch p1 p2 = do
    putStrLn "Start match"

mainLoop :: Socket -> GameState -> IO ()
mainLoop master gameState = do
    plCon <- accept master
    case handleIncomingConnection plCon gameState of
        (WaitingForTwoPlayers)                  -> mainLoop master WaitingForTwoPlayers
        waiting@(WaitingForAnotherPlayer p1)     -> do
            putStrLn $ "Connected to: " ++ show (fst p1)
            putStrLn "Waiting for a second player."
            mainLoop master waiting
        (GameReadyToStart p1 p2)            -> do
            -- Start match
            forkIO (prepareMatch p1 p2)
            mainLoop master WaitingForTwoPlayers

    where   handleIncomingConnection :: ClientConnection -> GameState -> GameState
            handleIncomingConnection newConn WaitingForTwoPlayers           = WaitingForAnotherPlayer newConn
            handleIncomingConnection newConn (WaitingForAnotherPlayer fst)  = GameReadyToStart fst newConn
            handleIncomingConnection _ st    = error $ "Illegal state: " ++ show st
                

main :: IO () 
main = do
    masterSock <- listenTo port
    mainLoop masterSock WaitingForTwoPlayers
    where port = "3333"
