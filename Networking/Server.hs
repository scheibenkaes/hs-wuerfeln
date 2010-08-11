module Networking.Server where 

import System.IO

import Network.Socket

defaultServer = "wettbewerb.linux-magazin.de"

defaultPort = "3333"

connectToServer :: String -> String -> IO Handle
connectToServer url port = do
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

sendAuth :: Handle -> String -> IO String
sendAuth conn name = do
    -- TODO sendLineToServer benutzen
    hPutStrLn conn $ "AUTH " ++ name
    s <- hGetLine conn
    return s 

sendLineToServer :: Handle -> String -> IO ()
sendLineToServer conn l = hPutStrLn conn l

readNextLineFromServer :: Handle -> IO String
readNextLineFromServer srv = do
    line <- hGetLine srv
    return line

