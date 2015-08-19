{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import Data.Monoid
import Control.Monad.State.Strict ()


data Point2D = Point2D { _px :: Int
                       , _py :: Int
                       } deriving (Show, Eq)

makeLenses ''Point2D

instance Monoid Point2D where
  mempty = Point2D 0 0
  (Point2D a a') `mappend` (Point2D b b') = Point2D (a+b) (a'+b')

data Direction = UpD | DownD | LeftD | RightD
  deriving (Show)

instance Monoid Direction where
  mempty                  = RightD
  DownD  `mappend` UpD    = DownD
  _      `mappend` UpD    = UpD
  UpD    `mappend` DownD  = UpD
  _      `mappend` DownD  = DownD
  RightD `mappend` LeftD  = RightD
  _      `mappend` LeftD  = LeftD
  LeftD  `mappend` RightD = LeftD
  _      `mappend` RightD = RightD

data Game = Game { _snake     :: [Point2D]
                 , _candy     :: Point2D
                 , _direction :: Direction
                 , _finished  :: Bool
                 } deriving (Show)

makeLenses ''Game

type Scene = (Int,Int)
