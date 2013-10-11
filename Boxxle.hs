{-# LANGUAGE FlexibleContexts #-} -- http://stackoverflow.com/questions/10865963/using-the-state-monad-to-hide-explicit-state

import Graphics.UI.SDL

{-import Graphics.UI.SDL.Mixer-}
import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Timer

-- game data
rooms = [
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
		,walls = -- TODO create a function that maps [[Int]] -> [Coord]
		[(Coord 3 3), (Coord 4 3), (Coord 5 3), (Coord 6 3), (Coord 7 3)
		,(Coord 3 4),                                        (Coord 7 4)	
		,(Coord 3 5),                                        (Coord 7 5),              (Coord 9 5), (Coord 10 5), (Coord 11 5)	
		,(Coord 3 6),                                        (Coord 7 6),              (Coord 9 6),               (Coord 11 6)	
		,(Coord 3 7),(Coord 4 7), (Coord 5 7),               (Coord 7 7), (Coord 8 7), (Coord 9 7),               (Coord 11 7)	
		            ,(Coord 4 8), (Coord 5 8),                                                                    (Coord 11 8)	
		            ,(Coord 4 9),                                         (Coord 8 9),                            (Coord 11 9)
		            ,(Coord 4 10),                                        (Coord 8 10),(Coord 9 10),(Coord 10 10),(Coord 11 10)	
		            ,(Coord 4 11),(Coord 5 11),(Coord 6 11),(Coord 7 11), (Coord 8 11)	
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
		,walls = -- TODO create a function that maps [[Int]] -> [Coord]
		[(Coord 3 3)]
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
}

type GameState = StateT GameData IO
type GameEnv = ReaderT GameConfig GameState


-- get/put/modify 
getGameData :: MonadState GameData m => m GameData
getGameData = get

putGameData :: MonadState GameData m => GameData -> m ()
putGameData = put

modifyGameData :: MonadState GameData m => (GameData -> GameData) -> m ()
modifyGameData = modify


getScreen :: MonadReader GameConfig m => m Surface
getScreen = liftM screen ask

getSprites :: MonadReader GameConfig m => m Surface
getSprites = liftM sprites ask


getPlayer :: MonadState GameData m => m Coord
getPlayer = gets player

getTimer :: MonadState GameData m => m Timer
getTimer = gets timer

getRoom :: MonadState GameData m => m Room
getRoom = gets room

putTimer :: MonadState GameData m => Timer -> m ()
putTimer t = modify $ \s -> s { timer = t }

modifyTimerM :: MonadState GameData m => (Timer -> m Timer) -> m ()
modifyTimerM act = getTimer >>= act >>= putTimer


-- main functions
newGame :: Int -> IO (GameConfig, GameData)
newGame lvl = do
	setVideoMode 640 480 32 []
	setCaption "Boxxle - Haskell" []
	screen <- getVideoSurface
	sprites <- loadBMP "img/boxxle.bmp"
	timer <- start defaultTimer
	return (GameConfig screen sprites, GameData timer room (startPos room) lvl)
		where room = rooms !! (lvl - 1)


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


levelUp :: GameData -> GameData
levelUp gd@GameData { player = player, level = level, room = room } = gd { level = newLevel, room = newRoom, player = (startPos newRoom) }
																		where 
																			newRoom = rooms !! (newLevel - 1)
																			newLevel = level + 1


handleWin :: GameData -> GameData
handleWin gd 	| isWin = levelUp gd
				| otherwise = gd
					where isWin = length (intersect (boxes currentRoom) (targets currentRoom)) == length (targets currentRoom)
						where currentRoom = (room gd)


collide :: Coord -> Coord -> Bool
collide c1 c2 = ((x c1) == (x c2)) && ((y c1) == (y c2))


offsetCoord :: Coord -> Move -> Coord
offsetCoord c@Coord{ x = x, y = y } move = c { x = x + (dx move), y = y + (dy move) } 


collideWithWorld :: Coord -> Room -> Bool
collideWithWorld c room = (foldr (||) False (map (collide c) (walls room)))
	

collideWithBoxes :: Coord -> Room -> Bool
collideWithBoxes c room = (foldr (||) False (map (collide c) (boxes room)))	


canBoxMove :: Coord -> Room -> Bool			
canBoxMove box room = not ((collideWithWorld box room) || (collideWithBoxes box room))


movePlayer :: Move -> GameData -> GameData
movePlayer move gd 	| collideWithWorld newPlayerPos (room gd) = gd
					| otherwise = gd { player = Coord { x = (x playerPos) + (dx move), y = (y playerPos) + (dy move)} }
					where 
						playerPos = (player gd)
						newPlayerPos = (offsetCoord playerPos move)


moveBox :: Move -> Coord -> Coord
moveBox move box@Coord { x = x, y = y } = box { x = x + (dx move), y = y + (dy move) }


checkBox :: Room -> Coord -> Move -> Coord -> Coord
checkBox room playerPos move box| collidedWithPlayer && (dir move) == UP && boxCanMove = moveBox move box
								| collidedWithPlayer && (dir move) == DOWN && boxCanMove = moveBox move box
								| collidedWithPlayer && (dir move) == LEFT && boxCanMove = moveBox move box
								| collidedWithPlayer && (dir move) == RIGHT && boxCanMove = moveBox move box
								| otherwise = box
									where 
										collidedWithPlayer = collide playerPos box -- player collided
										boxCanMove = canBoxMove newBoxPos room
											where newBoxPos = offsetCoord box move


checkBoxes :: Room -> Coord -> Move -> [Coord] -> [Coord]
checkBoxes room playerPos move boxes = map (checkBox room playerPos move) boxes


handleBoxes :: Move -> GameData -> GameData
handleBoxes move gd@GameData{ room = room@Room {boxes = boxes} } = gd { room = room { boxes = (checkBoxes room playerPos move boxes) } }
																where playerPos = offsetCoord (player gd) move


-- if the resulting move yields the player standing on a box, undo it
undoPlayer :: Move -> GameData -> GameData
undoPlayer move gd 	| collideWithBoxes playerPos (room gd) = gd { player = Coord { x = (x playerPos) - (dx move), y = (y playerPos) - (dy move)} }
					| otherwise = gd
					where 
						playerPos = (player gd)


handleKeyboard :: Event -> GameData -> GameData
handleKeyboard (KeyDown (Keysym SDLK_UP _ _)) gd = ((handleWin.undoPlayer move).(movePlayer move).(handleBoxes move)) gd
													where move = Move { dir = UP, dx = 0, dy = -1 }
handleKeyboard (KeyDown (Keysym SDLK_DOWN _ _)) gd = ((handleWin.undoPlayer move).(movePlayer move).(handleBoxes move)) gd
													where move = Move { dir = DOWN, dx = 0, dy = 1 }
handleKeyboard (KeyDown (Keysym SDLK_LEFT _ _)) gd = ((handleWin.undoPlayer move).(movePlayer move).(handleBoxes move)) gd
													where move = Move { dir = LEFT, dx = -1, dy = 0 }
handleKeyboard (KeyDown (Keysym SDLK_RIGHT _ _)) gd = ((handleWin.undoPlayer move).(movePlayer move).(handleBoxes move))  gd
													where move = Move { dir = RIGHT, dx = 1, dy = 0 }
handleKeyboard _ d = d


loop :: GameEnv ()
loop = do
	timer <- getTimer
	screen <- getScreen
	sprites <- getSprites
	pos <- getPlayer
	room <- getRoom

	modifyTimerM $ liftIO . start
	quit <- whileEvents $ modifyGameData . handleKeyboard

	liftIO $ do
		drawRoom screen sprites (tiles room)
		drawTargets screen sprites (targets room)
		drawBoxes screen sprites (boxes room)
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
	(gc, gd) <- newGame 1
	runLoop gc gd