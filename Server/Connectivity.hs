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

module Server.Connectivity where

import Network.Socket
import System.IO

import Networking.Messages

listenTo :: String -> IO Socket
listenTo port = withSocketsDo $ do
        addrInfos <-    getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                        Nothing (Just port)
        let serverAddr = head addrInfos

        sock <- socket (addrFamily serverAddr) Stream defaultProtocol
        bindSocket sock (addrAddress serverAddr)
        listen sock 6
        return sock

sendToClient :: Handle -> ServerMessage -> IO ()
sendToClient h msg = do
    hPutStrLn h (show msg)
