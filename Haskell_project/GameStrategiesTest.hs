{-# LANGUAGE TypeFamilies,FlexibleContexts,TemplateHaskell #-}
module GameStrategiesTest where
  
import Game
import GameStrategies
import Data.Bits
import Test.QuickCheck
import Test.QuickCheck.All

-- We implement the well-studied game Nim (http://en.wikipedia.org/wiki/Nim) to use as test game the minimax related functions.

type MaxDepth = Int
data Nim = Nim MaxDepth [Int] deriving Show
instance Game Nim where
  type GameState Nim = (Int,[Int])
  type Move Nim = (Int,Int)

  startState    game@(Nim _ xs) = (0,xs)
  move          game player (d,xs) (i,v) = (d+1,[if i==j then x-v else x | (j,x) <- zip [0..] xs])
  moves         game player (d,state) = [ (1 / fromIntegral x, (i,v)) | (i,x) <- zip [0..] state, v <- [1..x]]
  value         game@(Nim dmax _) player (d,state) = case (dmax==0 || d <= dmax) of
                  True -> theoreticalNimWinner (Nim 0 state) player
                  False -> error $ "Value should not be evaluated below level " ++ show dmax
  showGame      game state = show state
    
-- Theorem. In a normal Nim game, the player making the first move has a winning strategy if and only if the nim-sum of the sizes of the heaps is nonzero. Otherwise, the second player has a winning strategy
theoreticalNimWinner (Nim _ xs) p = (if (foldl xor 0 xs > 0) then 1 else -1) * (if p then 1 else -1)
maxTestNimListsum = 9 :: Int

anyNimGame = do
  a   <- choose (1,maxTestNimListsum)
  xs  <- genlist a
  return $ Nim 0 xs

genlist 0 = return []
genlist m = do
  x   <- choose (1,m)
  xs  <- genlist (m-x)
  return $ x:xs
  
prop_minimaxPicksWinningStrategyForNim = forAll (arbitrary :: Gen Bool) $ \startingPlayer -> 
                   forAll anyNimGame $ \game -> 
                   theoreticalNimWinner game startingPlayer == snd (minimax game (startTree game startingPlayer))

return []
main = $quickCheckAll

