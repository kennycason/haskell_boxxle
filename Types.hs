-- type defines
-- @author Kenny Cason
-- kennycason.com 2013

module Types 
where
import Graphics.UI.SDL
import Graphics.UI.SDL.TTF as TTFG
import Graphics.UI.SDL.Mixer

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Timer


data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Enum)

data Move = Move { 
    dir :: Direction
    ,dx :: Int
    ,dy :: Int 
}

data Coord = Coord { 
    x :: Int
    ,y :: Int 
} deriving (Eq)

data Room = Room { 
    tiles :: [[Int]]
    ,walls :: [Coord]
    ,boxes :: [Coord]
    ,targets :: [Coord]
    ,startPos :: Coord 
}

data GameData = GameData {
    timer :: Timer
    ,room :: Room
    ,player :: Coord
    ,level :: Int
}        

data GameConfig = GameConfig {
    screen :: Surface
    ,sprites :: Surface
    ,front :: Font
    ,music :: Music
}

type GameState = StateT GameData IO
type GameEnv = ReaderT GameConfig GameState
