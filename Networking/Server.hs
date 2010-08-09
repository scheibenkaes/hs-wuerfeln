module Networking.Server where 

import System.IO

import Network.Socket

server = "wettbewerb.linux-magazin.de"

port = "3333"

type ServerConnection = Handle

connectToServer :: String -> String -> IO ServerConnection
connectToServer url port = do
    sock <- openSocket url port
    h <- socket2Handle sock
    return h
    where
        openSocket url port = do
            addrInfo <- getAddrInfo Nothing (Just url) (Just port)
            let serverAddr = head addrInfo
            sock <- socket (addrFamily serverAddr) Stream defaultProtocol
            setSocketOption sock KeepAlive 1
            connect sock (addrAddress serverAddr)
            return sock
        socket2Handle :: Socket -> IO ServerConnection
        socket2Handle sock = do
            h <- socketToHandle sock ReadWriteMode
            hSetBuffering h LineBuffering
            return h 

sendAuth :: Handle -> String -> IO String
sendAuth conn name = do
    hPutStrLn conn $ "AUTH " ++ name
    s <- hGetLine conn
    return s 



