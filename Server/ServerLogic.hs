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
module ServerLogic where

import System.IO

data Player = Player {
      name          :: String
    , connection    :: Handle
}

data Match = Match {
      firstPlayer   :: Player
    , secondPlayer  :: Player
}

data Result = Result {
}

playMatch :: Player -> Player -> IO Result
playMatch p1 p2 = do
   turnTo p1 
   msg <- nextClientMessageFrom p1
    
