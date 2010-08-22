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
} deriving Show

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
        cfg = updateDefCfgWithParams opts defaultConfig
    return cfg

    where   updateDefCfgWithParams :: [Flag] -> Config -> Config
            updateDefCfgWithParams (x:xs) c = 
                case x of
                    (Server s)  -> updateDefCfgWithParams  xs (c {server = s})
                    (Port   p)  -> updateDefCfgWithParams  xs (c {port = p})
                    (Name   n)  -> updateDefCfgWithParams  xs (c {name = n})
                    _           -> c
            updateDefCfgWithParams [] c     = c
