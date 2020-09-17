{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module Game2048 where
import Game
import Data.List
type Width = Int
type Height = Int

data Game2048 = Game2048 Width Height
data Direction = L | R | U | D deriving (Eq,Show)
data Action = NewTile (Int,Int) Int | Slide Direction deriving (Eq,Show)

instance Game Game2048 where
  type GameState Game2048 = [[Int]]
  type Move Game2048 = Action
  
  showGame = undefined
  startState = undefined
  move = undefined
  moves = undefined
  value = undefined