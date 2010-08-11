module Networking.Server where 

import System.IO

import Network.Socket

defaultServer :: String
defaultServer = "wettbewerb.linux-magazin.de"

defaultPort :: String
defaultPort = "3333"

disconnectFromServer :: Handle -> IO ()
disconnectFromServer h = hClose h

connectToServer :: String -> String -> IO Handle
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

sendAuth :: Handle -> String -> IO String
sendAuth conn name = do
    sendLineToServer conn $ "AUTH " ++ name ++ " los gehts"
    s <- hGetLine conn
    return s 

sendLineToServer :: Handle -> String -> IO ()
sendLineToServer conn l = do
    hPutStrLn conn $ l 
    hFlush conn
    putStrLn  $ "Zum Server: " ++ l

readNextLineFromServer :: Handle -> IO String
readNextLineFromServer srv = do
    line <- hGetLine srv
    return line

