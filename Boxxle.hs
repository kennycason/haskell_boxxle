{-# LANGUAGE FlexibleContexts #-} -- http://stackoverflow.com/questions/10865963/using-the-state-monad-to-hide-explicit-state

-- @author Kenny Cason
-- kennycason.com 2013

import Graphics.UI.SDL

import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Timer

-- game data
-- levels from: http://www.gamefaqs.com/gameboy/585643-boxxle/faqs/52416
rooms = [

{-
Level 1
XXXXX
X...X
X.01X XXX
X.2.X X=X
XXX.XXX=X
 XX....=X
 X...X..X
 X...XXXX
 XXXXX-}
	Room {
		tiles = 
		[[1,1,1,1,1,0,0,0,0]
		,[1,0,0,0,1,0,0,0,0]
		,[1,0,0,0,1,0,1,1,1]
		,[1,0,0,0,1,0,1,0,1]
		,[1,1,1,0,1,1,1,0,1]
		,[0,1,1,0,0,0,0,0,1]
		,[0,1,0,0,0,1,0,0,1]
		,[0,1,0,0,0,1,1,1,1]
		,[0,1,1,1,1,1,0,0,0]
		]
		,walls = [] -- Autogenerate collidables from tiles[][]
		,boxes = [(Coord 2 2), (Coord 3 2), (Coord 2 3)]
		,targets = [(Coord 7 3), (Coord 7 4), (Coord 7 5)]
		,startPos = (Coord 1 1)
	}
{-
Level 2
XXXXXXXXXX
X==......X
X==0..X..X
X..X1XX.XX
X.2.....X
XXXXX.X.X
  X.3...X
  X.....X
  XXXXXXX-}
	,Room {
		tiles = 
		[[1,1,1,1,1,1,1,1,1,1]
		,[1,0,0,0,0,0,0,0,0,1]
		,[1,0,0,0,0,0,1,0,0,1]
		,[1,0,0,1,0,1,1,0,1,1]
		,[1,0,0,0,0,0,0,0,1,0]
		,[1,1,1,1,1,0,1,0,1,0]
		,[0,0,1,0,0,0,0,0,1,0]
		,[0,0,1,0,0,0,0,0,1,0]
		,[0,0,1,1,1,1,1,1,1,0]
		]
		,walls = []
		,boxes = [(Coord 3 2), (Coord 4 3), (Coord 2 4), (Coord 4 6)]
		,targets = [(Coord 1 1), (Coord 2 1), (Coord 1 2), (Coord 2 2)]
		,startPos = (Coord 1 1)
	}
{-
Level 3
 XXXX
XX..X
X.0.X
XX1.XX
XX.2.X
X=3..X
X==*=X
XXXXXX-}
	,Room {
		tiles = 
		[[0,1,1,1,1,0]
		,[1,1,0,0,1,0]
		,[1,0,0,0,1,0]
		,[1,1,0,0,1,1]
		,[1,1,0,0,0,1]
		,[1,0,0,0,0,1]
		,[1,0,0,0,0,1]
		,[1,1,1,1,1,1]
		]
		,walls = []
		,boxes = [(Coord 2 2), (Coord 2 3), (Coord 3 4), (Coord 2 5)]
		,targets = [(Coord 1 5), (Coord 1 6), (Coord 2 6), (Coord 4 6)]
		,startPos = (Coord 1 2)
	}
{- 
Level 4
 XXXXX
 X..XXX
 X.0..X
XXX.X.XX
X=X.X..X
X=1..X.X
X=...2.X
XXXXXXXX-}
	,Room {
		tiles = 
		[[0,1,1,1,1,1,0,0]
		,[0,1,0,0,1,1,1,0]
		,[0,1,0,0,0,0,1,0]
		,[1,1,1,0,1,0,1,1]
		,[1,0,1,0,1,0,0,1]
		,[1,0,0,0,0,1,0,1]
		,[1,0,0,0,0,0,0,1]
		,[1,1,1,1,1,1,1,1]
		]
		,walls = []
		,boxes = [(Coord 3 2), (Coord 2 5), (Coord 5 6)]
		,targets = [(Coord 1 4), (Coord 1 5), (Coord 1 6)]
		,startPos = (Coord 2 1)
	}
{- 
Level 5
 XXXXXXX
 X.....XXX
XX0XXX...X
X...1..2.X
X.==X.3.XX
XX==X...X
 XXXXXXXX-}
	,Room {
		tiles = 
		[[0,1,1,1,1,1,1,1,0,0]
		,[0,1,0,0,0,0,0,1,1,1]
		,[1,1,0,1,1,1,0,0,0,1]
		,[1,0,0,0,0,0,0,0,0,1]
		,[1,0,0,0,1,0,0,0,1,1]
		,[1,1,0,0,1,0,0,0,1,0]
		,[0,1,1,1,1,1,1,1,1,0]
		]
		,walls = []
		,boxes = [(Coord 2 2), (Coord 4 3), (Coord 6 3), (Coord 5 4)]
		,targets = [(Coord 2 4), (Coord 3 4), (Coord 2 5), (Coord 3 5)]
		,startPos = (Coord 2 1)
	}]
{- 
Level 6
       XXXX
       X..X
   XXXXX..X
XXXX......X
X...=XXX.XX
X.X.X....XX
X.X.0.1X=.X
X.X..*..X.X
X.=X2.3.X.X
XX....X.X.X
 X.XXX=...X
 X.....XXXX
 XXXXXXX-}

{- 
Level 7
   XXXXXXX
  XX..X..X
  X...X..X
  X0.1.2.X
  X.3XX..X
XXX.4.X.XX
X=====..X
XXXXXXXXX-}

{-
Level 8
   XXXXXX
 XXX....X
XX=.0XX.XX
X==1.2...X
X==.3.4.XX
XXXXXX..X
     XXXX-}

{- 
Level 10
  XXXXXX
  X....X
XXX012.X
X..3==.X
X.4===XX
XXXX..X
   XXXX-}

startLevel = 1

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
	setVideoMode 320 288 32 []
	setCaption "Boxxle - Haskell" []
	screen <- getVideoSurface
	sprites <- loadBMP "img/boxxle.bmp"
	timer <- start defaultTimer
	return (GameConfig screen sprites, GameData timer room (startPos room) lvl)
		where room = currentRoom {walls = foldTiles (tiles currentRoom) }
			where currentRoom = (rooms !! (lvl - 1))


levelUp :: GameData -> GameData
levelUp gd@GameData { player = player, level = level, room = room } = gd { level = newLevel, room = processedRoom, player = (startPos processedRoom) }
																		where 
																			newLevel = (level + 1) `mod` (length rooms)
																			processedRoom = nextRoom { walls = foldTiles (tiles nextRoom) }
																				where nextRoom = rooms !! (level `mod` (length rooms))


resetLevel :: GameData -> GameData
resetLevel gd@GameData { player = player, level = level, room = room } = gd { room = processedRoom, player = (startPos processedRoom) }
																		where processedRoom = resetRoom { walls = foldTiles (tiles resetRoom) }
																				where resetRoom = rooms !! ((level - 1) `mod` (length rooms))


handleWin :: GameData -> GameData
handleWin gd 	| isWin = levelUp gd
				| otherwise = gd
					where isWin = length (intersect (boxes currentRoom) (targets currentRoom)) == length (targets currentRoom)
						where currentRoom = (room gd)


foldTiles :: [[Int]] -> [Coord]
foldTiles tiles2d = concat (map foldTileRow (zip [0..] tiles2d))


foldTileRow :: (Int, [Int]) -> [Coord]
foldTileRow (row, tiles1d) = map toCoord $ filter pickOnes (zip [0..] tiles1d) where
    pickOnes (_, value) = value == 1
    toCoord (col, _) = Coord {x=col, y=row}


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


drawPlayer :: Surface -> Surface -> Int -> Int -> IO Bool
drawPlayer screen sprites x y = blitSurface sprites src screen dst
								where
									src = (getSpriteSheetOffset tPlayer)
									dst = Just (Rect (x * 32) (y * 32) 32 32)


drawBricks :: Surface -> Surface -> [Coord] -> IO()
drawBricks screen sprites bricks = mapM_ (\c -> drawSprite screen sprites tBrick ((x c) * 32) ((y c) * 32) ) bricks


drawBoxes :: Surface -> Surface -> [Coord] -> IO()
drawBoxes screen sprites boxes = mapM_ (\c -> drawSprite screen sprites tBox ((x c) * 32) ((y c) * 32) ) boxes


drawTargets :: Surface -> Surface -> [Coord] -> IO()
drawTargets screen sprites targets = mapM_ (\c -> drawSprite screen sprites tTarget ((x c) * 32) ((y c) * 32) ) targets



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
handleKeyboard (KeyDown (Keysym SDLK_r _ _)) gd = resetLevel gd
		
handleKeyboard _ d = d

loop :: GameEnv ()
loop = do
	timer 	<- getTimer
	screen 	<- getScreen
	sprites <- getSprites
	pos 	<- getPlayer
	room 	<- getRoom

	modifyTimerM $ liftIO . start
	quit <- whileEvents $ modifyGameData . handleKeyboard

	liftIO $ do
		bgRect	<- Just `liftM` getClipRect screen
		white 	<- mapRGB' screen 0xff 0xff 0xff
		fillRect screen bgRect white

		drawBricks screen sprites (walls room)
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
	mapRGB' = mapRGB . surfaceGetPixelFormat

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
	(gc, gd) <- newGame startLevel
	runLoop gc gd