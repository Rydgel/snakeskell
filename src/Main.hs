module Main where

import           Control.Lens
import           Control.Concurrent
import           System.Random
import           Data.Monoid ((<>))
import qualified UI.HSCurses.Curses as Curses
import           Types
import           Control.Monad.State.Strict


initialState :: Point2D -> Scene -> Game
initialState c (sh,sw) = Game (replicate 5 $ Point2D (sw `div` 2) (sh `div` 2)) c RightD False

finishGame :: State Game ()
finishGame = finished .= True

moveSnake :: State Game ()
moveSnake = modify' (\st -> st & snake .~ case st ^. direction of
  UpD    -> (st ^. snake ^. _head) <> Point2D 0 (-1) : st ^. snake ^. _init
  DownD  -> (st ^. snake ^. _head) <> Point2D 0 1 : st ^. snake ^. _init
  LeftD  -> (st ^. snake ^. _head) <> Point2D (-1) 0 : st ^. snake ^. _init
  RightD -> (st ^. snake ^. _head) <> Point2D 1 0 : st ^. snake ^. _init)

handleCollision :: Scene -> State Game ()
handleCollision (sh,sw) = do
  st <- get
  let xs = st ^. snake
  let posX = xs ^. _head ^. px
  let posY = xs ^. _head ^. py
  when (posX <= 0 || posX >= sw || posY <= 0 || posY >= sh || head xs `elem` tail xs) finishGame

eatCandy :: Point2D -> State Game ()
eatCandy newCandy = modify' (\st -> do
  let sn = st ^. snake
  if head sn == st ^. candy -- eating a candy
  then Game (sn ++ replicate 2 (last sn)) newCandy (st ^. direction) (st ^. finished)
  else st)

generateCandy :: Scene -> IO Point2D
generateCandy (sh, sw) = do
  randX <- randomRIO (1, sw-1)
  randY <- randomRIO (1, sh-1)
  return (Point2D randX randY)

inputDirection :: Curses.Key -> State Game ()
inputDirection Curses.KeyUp    = direction <>= UpD
inputDirection Curses.KeyDown  = direction <>= DownD
inputDirection Curses.KeyLeft  = direction <>= LeftD
inputDirection Curses.KeyRight = direction <>= RightD
inputDirection _               = modify' id

drawBorders :: Curses.Window -> Scene -> IO ()
drawBorders window (sh,sw) =
  mapM_ (\(x,y) -> Curses.mvWAddStr window y x "▨") borderPoints where
    cond x y = x == 0 || x == sw || y == 0 || y == sh
    borderPoints = [(x, y) | x <- [0..sw], y <- [0..sh], cond x y]

drawScene :: Game -> Curses.Window -> Scene -> IO ()
drawScene (Game sn c _ _) window scene = do
  Curses.wclear window
  drawBorders window scene -- draw borders
  Curses.mvWAddStr window (c ^. py) (c ^. px) "@" -- draw candy
  mapM_ (\s -> Curses.mvWAddStr window (s ^. py) (s ^. px) "#") sn -- draw snake
  Curses.wRefresh window

hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

getKeyEvent :: Scene -> StateT Game IO ()
getKeyEvent scene = do
  newCandy <- liftIO $ generateCandy scene
  char <- liftIO Curses.getch
  hoistState $ case Curses.decodeKey char of
    Curses.KeyChar 'q' -> finishGame -- quit the game
    k -> inputDirection k >> moveSnake >> handleCollision scene >> eatCandy newCandy

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
