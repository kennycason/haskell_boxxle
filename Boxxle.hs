{-# LANGUAGE FlexibleContexts #-}
-- http://stackoverflow.com/questions/10865963/using-the-state-monad-to-hide-explicit-state
import Graphics.UI.SDL
import Graphics.UI.SDL.Video

{-import Graphics.UI.SDL.Mixer-}

import Data.Word
import Data.Maybe

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Timer

-- game data
levels = [
	Room {
		tiles = 
		[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,1,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0]
		,[0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0]
		,[0,0,0,1,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,0]
		,[0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0]
		,[0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0]
		,[0,0,0,0,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0]
		,[0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		]
		,boxes = [(Coord 5 5), (Coord 6 5), (Coord 5 6)]
		,targets = [(Coord 10 6), (Coord 10 7), (Coord 10 8)]
		,startPos = (Coord 4 4)
	}
	,Room {
		tiles = 
		[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0]
		,[0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0]
		,[0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0]
		,[0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
		]
		,boxes = [(Coord 5 7), (Coord 8 7), (Coord 7 6), (Coord 7 5)]
		,targets = [(Coord 6 6), (Coord 7 6), (Coord 6 7), (Coord 7 7)]
		,startPos = (Coord 6 4)
	}]

tEmpty 	= 0
tBrick 	= 1
tBox 	= 2
tPlayer = 3
tTarget	= 4

-- type defines
data Direction = Up | Down | Left | Right deriving (Eq, Enum)

data Move = Move { dir :: Direction, newPos :: Coord }

data Coord = Coord { x :: Int, y :: Int }

data Room = Room { tiles :: [[Int]], boxes :: [Coord], targets :: [Coord], startPos :: Coord }

data GameData = GameData {
	timer :: Timer,
	currentRoom :: Room,
	playerPos :: Coord
}

data GameConfig = GameConfig {
	screen :: Surface,
	sprites :: Surface
}

type GameState = StateT GameData IO
type GameEnv = ReaderT GameConfig GameState


-- getters/setters 

getPlayerPos :: MonadState GameData m => m Coord
getPlayerPos = liftM playerPos get

putPlayerPos :: MonadState GameData m => Coord -> m ()
putPlayerPos t = modify $ \s -> s { playerPos = t }

modifyPlayerPos :: MonadState GameData m => (Coord -> Coord) -> m ()
modifyPlayerPos fn = liftM fn getPlayerPos >>= putPlayerPos


getTimer :: MonadState GameData m => m Timer
getTimer = liftM timer get

putTimer :: MonadState GameData m => Timer -> m ()
putTimer t = modify $ \s -> s { timer = t }

modifyTimerM :: MonadState GameData m => (Timer -> m Timer) -> m ()
modifyTimerM act = getTimer >>= act >>= putTimer

getRoom :: MonadState GameData m => m Room
getRoom = liftM currentRoom get

getScreen :: MonadReader GameConfig m => m Surface
getScreen = liftM screen ask

getSprites :: MonadReader GameConfig m => m Surface
getSprites = liftM sprites ask


-- main functions
newGame :: Int -> IO (GameConfig, GameData)
newGame lvl = do
	setVideoMode 640 480 32 []
	setCaption "Boxxle - Haskell" []
	screen <- getVideoSurface
	sprites <- loadBMP "img/boxxle.bmp"
	timer <- start defaultTimer
	return (GameConfig screen sprites, GameData timer level (startPos level))
		where level = levels !! (lvl - 1)


getSpriteSheetOffset :: Int -> Maybe Rect
getSpriteSheetOffset n = Just (Rect offx offy 32 32)
							where 
								offx = n * 32
								offy = 0


drawSprite :: Surface -> Surface -> Int -> Int -> Int -> IO Bool
drawSprite screen sprites n x y = blitSurface 
											sprites (getSpriteSheetOffset n) 
											screen dst
													where dst = Just (Rect x y 32 32)


drawRoom :: Surface -> Surface -> [[Int]] -> IO()
drawRoom screen sprites room = mapM_ (\r -> drawRow screen sprites r) [0..14]
								where drawRow screen sprites r = 
									mapM_ (\(x, n) -> drawSprite screen sprites n (x * 32) (r * 32)) (coord (room !! r))
										where coord row = map (\i -> (i, row !! i)) [0..19]


drawPlayer :: Surface -> Surface -> Int -> Int -> IO Bool
drawPlayer screen sprites x y = blitSurface sprites src screen dst
								where
									src = (getSpriteSheetOffset tPlayer)
									dst = Just (Rect (x * 32) (y * 32) 32 32)


drawBoxes :: Surface -> Surface -> [Coord] -> IO()
drawBoxes screen sprites boxes = mapM_ (\c -> drawSprite screen sprites tBox ((x c) * 32) ((y c) * 32) ) boxes


drawTargets :: Surface -> Surface -> [Coord] -> IO()
drawTargets screen sprites targets = mapM_ (\c -> drawSprite screen sprites tTarget ((x c) * 32) ((y c) * 32) ) targets


collide :: Coord -> Coord -> Bool
collide c1 c2 = ((x c1) == (x c2)) && ((y c1) == (y c2))


movePlayer :: Move -> GameData -> GameData
movePlayer move gamedata  = gamedata


handleKeyboard2 :: Event -> GameData -> GameData
handleKeyboard2 (KeyDown (Keysym SDLK_UP _ _)) gd@GameData { playerPos = Coord { x = x, y = y } } = gd { playerPos = Coord{ x = x, y = y - 1 } }


handleKeyboard :: Event -> Coord -> Coord
handleKeyboard (KeyDown (Keysym SDLK_UP _ _)) c@Coord { x = x, y = y } = c
handleKeyboard (KeyDown (Keysym SDLK_DOWN _ _)) c@Coord { x = x, y = y } = c
handleKeyboard (KeyDown (Keysym SDLK_LEFT _ _)) c@Coord { x = x, y = y } = c
handleKeyboard (KeyDown (Keysym SDLK_RIGHT _ _)) c@Coord { x = x, y = y } = c


handleKeyboard (KeyUp (Keysym SDLK_UP _ _)) c@Coord { x = x, y = y } = c { x = x, y = y - 1 }
handleKeyboard (KeyUp (Keysym SDLK_DOWN _ _)) c@Coord { x = x, y = y } = c { x = x, y = y + 1 }
handleKeyboard (KeyUp (Keysym SDLK_LEFT _ _)) c@Coord { x = x, y = y } = c { x = x - 1, y = y }
handleKeyboard (KeyUp (Keysym SDLK_RIGHT _ _)) c@Coord { x = x, y = y } = c { x = x + 1, y = y }

handleKeyboard _ d = d


loop :: GameEnv ()
loop = do
	timer <- getTimer
	screen <- getScreen
	sprites <- getSprites
	pos <- getPlayerPos
	room <- getRoom

	modifyTimerM $ liftIO . start
	quit <- whileEvents $ modifyPlayerPos . handleKeyboard

  	-- modifyBoxes $ move $ 

	liftIO $ do
		drawRoom screen sprites (tiles room)
		drawBoxes screen sprites (boxes room)
		drawTargets screen sprites (targets room)
		drawPlayer screen sprites (x pos) (y pos)
		Graphics.UI.SDL.flip screen

		ticks <- getTimerTicks timer
		when (ticks < secsPerFrame) $ do
			delay $ secsPerFrame - ticks
	unless quit loop
 where
	framesPerSecond = 30
	secsPerFrame = 1000 `div` framesPerSecond

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
	event <- liftIO pollEvent
	case event of
		Quit -> return True
		NoEvent -> return False
		_ -> do
			act event
			whileEvents act


runLoop :: GameConfig -> GameData -> IO ()
runLoop = evalStateT . runReaderT loop


main = withInit [InitEverything] $ do -- withInit calls quit for us.
	(gConf, gData) <- newGame 1
	runLoop gConf gData