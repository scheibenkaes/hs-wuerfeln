module Networking.Server where 

import Network.Socket
import System.IO

server = "wettbewerb.linux-magazin.de"

port = "3333"

type ServerConnection = IO Handle

connectToServer :: String -> String -> ServerConnection
connectToServer url port = do
    addrInfo <- getAddrInfo Nothing (Just url) (Just port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)
    h <- socketToHandle sock WriteMode
    hSetBuffering h LineBuffering

    return h

