module Networking.Server where 

import System.IO

import Network.Socket

defaultServer :: String
defaultServer = "wettbewerb.linux-magazin.de"

defaultPort :: String
defaultPort = "3333"

disconnectFromServer :: Socket -> IO ()
disconnectFromServer s = sClose s


connectToServer :: String -> String -> IO Socket
connectToServer url port = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just url) (Just port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)
    return sock

sendAuth :: Socket -> String -> IO String
sendAuth conn name = do
    sendLineToServer conn $ "AUTH " ++ name ++ " los gehts"
    s <- readNextLineFromServer conn
    return s 

sendLineToServer :: Socket -> String -> IO ()
sendLineToServer conn l = do
    send conn $ l ++ "\n"
    putStrLn  $ "Zum Server: " ++ l

readNextLineFromServer :: Socket -> IO String
readNextLineFromServer srv = do
    text <- recv srv 512
    let line = (lines text) !! 0
    return line
