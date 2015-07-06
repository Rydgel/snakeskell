module Types where

import Data.Semigroup


data Point2D = Point2D { px :: Int
                       , py :: Int
                       } deriving (Show, Eq)

instance Semigroup Point2D where
  (Point2D a a') <> (Point2D b b') = Point2D (a+b) (a'+b')

data Direction = UpD | DownD | LeftD | RightD
  deriving (Show)

instance Semigroup Direction where
  DownD  <> UpD    = DownD
  _      <> UpD    = UpD
  UpD    <> DownD  = UpD
  _      <> DownD  = DownD
  RightD <> LeftD  = RightD
  _      <> LeftD  = LeftD
  LeftD  <> RightD = LeftD
  _      <> RightD = RightD

data State = State { snake     :: [Point2D]
                   , candy     :: Point2D
                   , direction :: Direction
                   , finished  :: Bool
                   } deriving (Show)

type Scene = (Int,Int)
