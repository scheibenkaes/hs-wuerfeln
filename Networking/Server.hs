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
module Networking.Server where 

import Network.Socket
import System.IO

import Networking.Messages

type ServerConnection = Handle

disconnectFromServer :: ServerConnection -> IO ()
disconnectFromServer s = hClose s

connectToServer :: String -> String -> IO ServerConnection
connectToServer url port = withSocketsDo $ do
    sock <- openSocket 
    h <- socket2Handle sock
    return h
    where
        openSocket = do
            addrInfo <- getAddrInfo Nothing (Just url) (Just port)
            let serverAddr = head addrInfo
            sock <- socket (addrFamily serverAddr) Stream defaultProtocol
            setSocketOption sock KeepAlive 1
            connect sock (addrAddress serverAddr)
            return sock
        socket2Handle :: Socket -> IO Handle
        socket2Handle sock = do
            h <- socketToHandle sock ReadWriteMode
            hSetBuffering h LineBuffering
            return h 

sendAuth :: ServerConnection -> String -> IO ()
sendAuth conn name = do
    let auth = AUTH name ""
    sendLineToServer conn $ show auth

sendLineToServer :: ServerConnection -> String -> IO ()
sendLineToServer conn l = do
    hPutStrLn conn l
    return ()

readNextLineFromServer :: ServerConnection -> IO String
readNextLineFromServer srv = do
    line <- hGetLine srv
    return line


putMsg :: ServerMessage -> IO ()
putMsg msg = 
    putStrLn $ show msg 


getNextMsg :: ServerConnection -> IO ServerMessage
getNextMsg srv = do 
    msg <- readNextLineFromServer srv
    return $ parseServerMessage msg
