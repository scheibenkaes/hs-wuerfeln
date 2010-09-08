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
module Config where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment

appName :: String
appName = "pontiac"

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

defaultConfig :: Config
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
    let (opts, nopts, _) = getOpt Permute options args
        cfg = updateDefCfgWithParams opts defaultConfig
        cfgWLogic = updateLogic nopts cfg
    return cfgWLogic

    where   updateDefCfgWithParams :: [Flag] -> Config -> Config
            updateDefCfgWithParams (x:xs) c = 
                case x of
                    (Server s)  -> updateDefCfgWithParams  xs (c {server = s})
                    (Port   p)  -> updateDefCfgWithParams  xs (c {port = p})
                    (Name   n)  -> updateDefCfgWithParams  xs (c {name = n})
                    _           -> c
            updateDefCfgWithParams [] c     = c

            updateLogic :: [String] -> Config -> Config
            updateLogic [] cfg = cfg
            updateLogic ns cfg =
                cfg { logic = ns !! 0 }
                
