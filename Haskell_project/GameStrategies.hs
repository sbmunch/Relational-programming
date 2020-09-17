{-# LANGUAGE FlexibleContexts #-}
module GameStrategies where
import Data.List (intersperse)
import Game2048
import Game
data Tree m v   = Tree v [(m,Tree m v)] 
type GTree g    = Tree (Weight,Move g) (Player,GameState g) 

type Depth = Int
instance (Show m, Show v) => Show (Tree m v) where
  show (Tree x xs) = "Tree " ++ show x ++ " ["++ sub' ++"]"
    where 
      sub = concat $ intersperse ",\n" $ map show xs 
      sub' = if null sub 
                then "" 
                else "\n" ++ (unlines $ map ("  " ++ ) $ lines sub)  

startTree :: Game g => g -> Player -> GTree g
startTree g p =  tree g (p, startState g)

tree :: Game g => g -> (Player, GameState g) -> GTree g
tree = undefined
  
takeTree :: Depth -> Tree m v -> Tree m v
takeTree = undefined

minimax :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minimax = undefined

expectimax :: Game g => g -> GTree g -> (Maybe (Move g), Value)
expectimax = undefined