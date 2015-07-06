{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Concurrent
import           Control.Monad      (unless)
import           Data.Semigroup
import           System.Random
import qualified UI.HSCurses.Curses as Curses
import           Types


initialState :: Point2D -> Scene -> State
initialState c (sh,sw) = State (replicate 5 $ Point2D (sw `div` 2) (sh `div` 2)) c RightD False

finishGame :: State -> State
finishGame (State sn c d _) = State sn c d True

moveSnake :: State -> State
moveSnake (State s c UpD f) = State (head s <> Point2D 0 (-1) : init s) c UpD f
moveSnake (State s c DownD f) = State (head s <> Point2D 0 1 : init s) c DownD f
moveSnake (State s c LeftD f) = State (head s <> Point2D (-1) 0 : init s) c LeftD f
moveSnake (State s c RightD f) = State (head s <> Point2D 1 0 : init s) c RightD f

handleCollision :: Scene -> State -> State
handleCollision (sh,sw) st @ (State xs _ _ _)
  | posX <= 0 || posX >= sw = finishGame st
  | posY <= 0 || posY >= sh = finishGame st
  -- todo collision snake itself, basically if the head collides one elem of the tail
  | otherwise = st
    where posX = px $ head xs
          posY = py $ head xs

eatCandy :: Point2D -> State -> State
eatCandy newCandy st @ (State sn c d f)
  | head sn == c = State (sn ++ replicate 2 (last sn)) newCandy d f
  | otherwise = st

generateCandy :: Scene -> IO Point2D
generateCandy (sh, sw) = do
  randX <- randomRIO (1, sw-1)
  randY <- randomRIO (1, sh-1)
  return (Point2D randX randY)

inputDirection :: Curses.Key -> State -> State
inputDirection Curses.KeyUp (State sn c d f) = State sn c (d <> UpD) f
inputDirection Curses.KeyDown (State sn c d f) = State sn c (d <> DownD) f
inputDirection Curses.KeyLeft (State sn c d f) = State sn c (d <> LeftD) f
inputDirection Curses.KeyRight (State sn c d f) = State sn c (d <> RightD) f
inputDirection _ s = s

drawBorders :: Curses.Window -> Scene -> IO ()
drawBorders window (sh,sw) =
  mapM_ (\(x,y) -> Curses.mvWAddStr window y x "▨") borderPoints where
    cond x y = x == 0 || x == sw || y == 0 || y == sh
    borderPoints = [(x, y) | x <- [0..sw], y <- [0..sh], cond x y]

drawScene :: State -> Curses.Window -> Scene -> IO ()
drawScene (State sn c _ _) window scene = do
  Curses.wclear window
  drawBorders window scene -- draw borders
  Curses.mvWAddStr window (py c) (px c) "@" -- draw candy
  mapM_ (\s -> Curses.mvWAddStr window (py s) (px s) "#") sn -- draw snake
  Curses.wRefresh window

getKeyEvent :: State -> Scene -> IO State
getKeyEvent s scene = do
  newCandy <- generateCandy scene
  Curses.getch >>= \ch -> case Curses.decodeKey ch of
    Curses.KeyChar 'q' -> return $ finishGame s -- quit the game
    _k -> do
      let changeDirection = inputDirection _k
      return $ eatCandy newCandy $ handleCollision scene $ moveSnake $ changeDirection s

tick :: State -> Curses.Window -> IO ()
tick s w = do
  _ <- Curses.flushinp -- flush input
  (sh,sw) <- Curses.scrSize
  let scene = (sh-2,sw-1)
  drawScene s w scene
  threadDelay 80000
  newState <- getKeyEvent s scene
  unless (finished newState) $ tick newState w

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
  tick (initialState initialCandy scene) window
  killCurses
