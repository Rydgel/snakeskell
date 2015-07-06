{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Concurrent
import           Data.Monoid
import           System.Random
import           Control.Monad (unless)
import qualified UI.HSCurses.Curses as Curses

data Point2D = Point2D { px :: Int
                       , py :: Int
                       } deriving (Show, Eq)

instance Monoid Point2D where
  mappend (Point2D a a') (Point2D b b') = Point2D (a+b) (a'+b')
  mempty = Point2D 0 0

data Direction = UpD | DownD | LeftD | RightD deriving (Show)

data State = State { snake :: [Point2D]
                   , candy :: Point2D
                   , direction :: Direction
                   , finished :: Bool
                   } deriving (Show)

initialState :: Point2D -> (Int,Int) -> State
initialState c (sh,sw) = State (replicate 5 $ Point2D (sw `div` 2) (sh `div` 2)) c RightD False

moveSnake :: State -> State
-- todo prevent going behind
moveSnake (State s @ (x:_) c UpD f) = State (x <> Point2D 0 (-1) : init s) c UpD f
moveSnake (State s @ (x:_) c DownD f) = State (x <> Point2D 0 1 : init s) c DownD f
moveSnake (State s @ (x:_) c LeftD f) = State (x <> Point2D (-1) 0 : init s) c LeftD f
moveSnake (State s @ (x:_) c RightD f) = State (x <> Point2D 1 0 : init s) c RightD f
moveSnake st @ (State [] _ _ _) = st

handleCollision :: (Int,Int) -> State -> State
handleCollision (sh,sw) st @ (State (x:xs) c d _)
  | posX <= 0 || posX >= sw = State (x:xs) c d True
  | posY <= 0 || posY >= sh = State (x:xs) c d True
  -- todo collision snake itself, basically if the head collides one elem of the tail
  | otherwise = st
    where posX = px x
          posY = py x
handleCollision (_, _) st @ (State [] _ _ _) = st


eatCandy :: Point2D -> State -> State
eatCandy newCandy st @ (State sn @ (x:_) c d f)
  | x == c = State (sn ++ replicate 2 (last sn)) newCandy d f
  | otherwise = st
eatCandy _ st @ (State [] _ _ _) = st

generateCandy :: (Int,Int) -> IO Point2D
generateCandy (sh, sw) = do
  randX <- randomRIO (1, sw-1)
  randY <- randomRIO (1, sh-1)
  return (Point2D randX randY)

inputDirection :: Curses.Key -> State -> State
inputDirection Curses.KeyUp (State sn c _ f) = State sn c UpD f
inputDirection Curses.KeyDown (State sn c _ f) = State sn c DownD f
inputDirection Curses.KeyLeft (State sn c _ f) = State sn c LeftD f
inputDirection Curses.KeyRight (State sn c _ f) = State sn c RightD f
inputDirection _ s = s

drawBorders :: Curses.Window -> (Int,Int) -> IO ()
drawBorders window (sh,sw) =
  mapM_ (\(x,y) -> Curses.mvWAddStr window y x "▨") borderPoints where
    cond x y = x == 0 || x==sw || y==0 || y==sh
    borderPoints = [(x, y) | x <- [0..sw], y <- [0..sh], cond x y]

drawScene :: State -> Curses.Window -> (Int,Int) -> IO ()
drawScene (State sn c _ _) window scene = do
  Curses.wclear window
  drawBorders window scene -- draw borders
  Curses.mvWAddStr window (py c) (px c) "@" -- draw candy
  mapM_ (\s -> Curses.mvWAddStr window (py s) (px s) "#") sn -- draw snake
  Curses.wRefresh window

tick :: State -> Curses.Window -> IO ()
tick s w = do
  _ <- Curses.flushinp -- flush input
  (sh,sw) <- Curses.scrSize
  let scene = (sh-1,sw-1)
  drawScene s w scene
  threadDelay 80000
  newCandy <- generateCandy scene
  -- todo refactor this shit
  Curses.getch >>= \ch -> case Curses.decodeKey ch of
      Curses.KeyChar 'q' -> return () -- quit the game
      _k -> do
        let changeDirection = inputDirection _k
        let newState = eatCandy newCandy $ handleCollision scene $ moveSnake $ changeDirection s
        unless (finished newState) $ tick newState w

main :: IO ()
main = do
  window <- Curses.initScr
  (sh,sw) <- Curses.scrSize
  let scene = (sh-1,sw-1)
  _ <- Curses.cursSet Curses.CursorInvisible
  Curses.timeout 0
  Curses.cBreak True
  Curses.keypad Curses.stdScr True
  Curses.echo False
  initialCandy <- generateCandy scene
  tick (initialState initialCandy scene) window
  Curses.resetParams
  Curses.endWin
