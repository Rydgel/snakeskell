{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Lens
import           Control.Concurrent
import           System.Random
import           Data.Monoid ((<>))
import qualified UI.HSCurses.Curses as Curses
import           Types
import           Control.Monad.State.Strict


initialState :: Point2D -> Scene -> Game
initialState c (sh,sw) =
  Game (replicate 5 $ Point2D (sw `div` 2) (sh `div` 2)) c RightD False False

finishGame :: State Game ()
finishGame = finished .= True

togglePauseGame :: State Game ()
togglePauseGame = pause %= not

moveSnake :: State Game ()
moveSnake = do
  isPaused <- use pause
  dir <- use direction
  unless isPaused $ snake %= moveSnakeWithDirection dir

moveSnakeWithDirection :: Direction -> [Point2D] -> [Point2D]
moveSnakeWithDirection UpD    s = head s <> Point2D 0 (-1) : init s
moveSnakeWithDirection DownD  s = head s <> Point2D 0 1 : init s
moveSnakeWithDirection LeftD  s = head s <> Point2D (-1) 0 : init s
moveSnakeWithDirection RightD s = head s <> Point2D 1 0 : init s

handleCollision :: Scene -> State Game ()
handleCollision (sh,sw) = do
  xs <- use snake
  let posX = head xs ^. px
  let posY = head xs ^. py
  when (posX <= 0 || posX >= sw || posY <= 0 || posY >= sh || head xs `elem` tail xs)
    finishGame

eatCandy :: Point2D -> State Game ()
eatCandy newCandy = do
  sn <- use snake
  c  <- use candy
  when (head sn == c) $ do
    candy .= newCandy
    snake .= (sn ++ replicate 2 (last sn))

generateCandy :: Scene -> IO Point2D
generateCandy (sh, sw) = do
  randX <- randomRIO (1, sw-1)
  randY <- randomRIO (1, sh-1)
  return $ Point2D randX randY

inputDirection :: Curses.Key -> State Game ()
inputDirection Curses.KeyUp    = direction <>= UpD
inputDirection Curses.KeyDown  = direction <>= DownD
inputDirection Curses.KeyLeft  = direction <>= LeftD
inputDirection Curses.KeyRight = direction <>= RightD
inputDirection _               = return ()

drawBorders :: Curses.Window -> Scene -> IO ()
drawBorders window (sh,sw) =
  mapM_ (\(x,y) -> Curses.mvWAddStr window y x "▨") borderPoints where
    cond x y = x == 0 || x == sw || y == 0 || y == sh
    borderPoints = [(x, y) | x <- [0..sw], y <- [0..sh], cond x y]

hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

getKeyEvent :: Scene -> StateT Game IO ()
getKeyEvent scene = do
  newCandy <- liftIO $ generateCandy scene
  char <- liftIO Curses.getch
  hoistState $ case Curses.decodeKey char of
    Curses.KeyChar 'q' -> finishGame -- quit the game
    Curses.KeyChar 'p' -> togglePauseGame -- pause the game
    k                  -> do
                            inputDirection k
                            moveSnake
                            handleCollision scene
                            eatCandy newCandy

tick :: Curses.Window -> StateT Game IO ()
tick w = do
  st <- get
  _ <- liftIO Curses.flushinp -- flush input
  (sh,sw) <- liftIO Curses.scrSize
  let scene = (sh-2,sw-1)
  liftIO $ drawScene st w scene
  liftIO $ threadDelay 80000
  newState <- execStateT (lift (getKeyEvent scene)) st
  unless (newState ^. finished) $ tick w

drawScene :: Game -> Curses.Window -> Scene -> IO ()
drawScene Game{..} window scene = do
  Curses.wclear window
  drawBorders window scene -- draw borders
  Curses.mvWAddStr window (_candy ^. py) (_candy ^. px) "@" -- draw candy
  mapM_ (\s -> Curses.mvWAddStr window (s ^. py) (s ^. px) "#") _snake -- draw snake
  Curses.wRefresh window

initCurses :: IO (Curses.Window, Scene)
initCurses = do
  window <- Curses.initScr
  _ <- Curses.cursSet Curses.CursorInvisible
  Curses.timeout 0
  Curses.cBreak True
  Curses.keypad Curses.stdScr True
  Curses.echo False
  (sh,sw) <- Curses.scrSize
  let scene = (sh-2,sw-1)
  return (window, scene)

killCurses :: IO ()
killCurses = do
  Curses.resetParams
  Curses.endWin

main :: IO ()
main = do
  (window, scene) <- initCurses
  initialCandy <- generateCandy scene
  evalStateT (tick window) (initialState initialCandy scene)
  killCurses
