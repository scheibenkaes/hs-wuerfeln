module Networking.Server where 

import System.IO

import Network.Socket

defaultServer :: String
defaultServer = "wettbewerb.linux-magazin.de"

defaultPort :: String
defaultPort = "3333"

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

sendAuth :: ServerConnection -> String -> IO String
sendAuth conn name = do
    sendLineToServer conn $ "AUTH " ++ name ++ " los gehts"
    s <- readNextLineFromServer conn
    return s 

sendLineToServer :: ServerConnection -> String -> IO ()
sendLineToServer conn l = do
    hPutStrLn conn l
    return ()
    --putStrLn  $ "Zum Server: " ++ l

readNextLineFromServer :: ServerConnection -> IO String
readNextLineFromServer srv = do
    line <- hGetLine srv
    return line
