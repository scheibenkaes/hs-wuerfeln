module Config where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment

appName :: String
appName = "olddice"

defaultServer :: String
defaultServer = "wettbewerb.linux-magazin.de"

defaultPort :: String
defaultPort = "3333"

data Flag = 
      Logic     String
    | Server    String
    | Port      String
    | Name      String
    deriving Show

data Config = Config {
      logic     :: String
    , server    :: String
    , port      :: String
    , name      :: String
}

defaultConfig = Config {
      logic     = "x"
    , server    = defaultServer
    , port      = defaultPort
    , name      = appName
}

options :: [OptDescr Flag]
options = 
    [ 
        Option ['n'] ["name"]   (OptArg nme "NAME")    "Player name",
        Option ['s'] ["server"] (OptArg serv "SERVER")  "Server address",
        Option ['p'] ["port"]   (OptArg prt "PORT")     "Port number"
    ]
    where   serv, prt, nme :: Maybe String -> Flag
            serv    = Server    . fromMaybe defaultServer
            prt     = Port      . fromMaybe defaultPort
            nme     = Name      . fromMaybe appName

getConfig :: IO Config
getConfig = do
    args <- getArgs
    let (opts, nopts, errs) = getOpt Permute options args
    print opts
    let logic = getLogic $ fromArgs nopts
        name = optName $ opts !! 0
        srv  = optServ $ opts !! 1
        port = optPort $ opts !! 2
