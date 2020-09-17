{-# LANGUAGE TypeFamilies,FlexibleContexts,TemplateHaskell #-}
module Game2048Test where

import Test.QuickCheck
import Test.QuickCheck.All
import Game
import Game2048
import Data.Maybe (fromJust)
import Data.Map (fromListWith,toList)
import Data.List (nub, elem)
import Debug.Trace
anyLength = choose (1,9) :: Gen Int

-- startState:
--    Tjek at det er 100% nuller
prop_startState_allZeros =  forAll anyLength $ \w ->
                            forAll anyLength $ \h -> 
                            let g = Game2048 w h in (nub $ concat (startState g)) == [0]


--    Tjek bredde og højde 
prop_startState_boardDimensions =   forAll anyLength $ \w ->
                                    forAll anyLength $ \h -> 
                                    let g = Game2048 w h
                                        s = startState g
                                    in length s== h && (nub (map length s)) == [w] 

-- Givet et spil, lav en tilfældig sekvens af moves... 
anyValidMoveSequence :: (Game g) => g -> Player -> GameState g -> Int -> Gen [(GameState g, Player, Move g, GameState g)]
anyValidMoveSequence g p s 0 = return []
anyValidMoveSequence g p s maxLen = do  let ms = moves g p s
                                        case ms of
                                            [] -> return []
                                            _  -> do
                                                  i <- choose (0,length ms - 1)
                                                  let m = snd $ ms !! i
                                                  let x = (s,p,m,move g p s m)
                                                  xs <- anyValidMoveSequence g (not p) (move g p s m) (maxLen-1)
                                                  return $ x:xs


--move
--    Tjek at sum ændrer sig med value med computer's moves!
--    Tjek at sum er uændret ved player's moves!

testmove g stateBefore True _ stateAfter = (sum $ concat stateBefore) == (sum $ concat stateAfter)
testmove g stateBefore False (NewTile _ v) stateAfter = (sum $ concat stateBefore) + v == (sum $ concat stateAfter)


prop_move = forAll anyLength $ \w ->
            forAll anyLength $ \h -> 
            sized $ \n -> let g = Game2048 w h in
            return $ forAll (anyValidMoveSequence g False (startState g) n) $ \xs ->
            and [ testmove g  s1 np nm s2 | (s1,np,nm,s2) <- xs]
            

-- Tjek at vægtene summer op til ca. 1
prop_move2 = forAll anyLength $ \w ->
             forAll anyLength $ \h -> 
             sized $ \n -> let g = Game2048 w h in
             return $ forAll (anyValidMoveSequence g False (startState g) n) $ \xs ->
             and [ abs( sum (map fst (moves g np s1)) - 1) < 0.001 | (s1,np,nm,s2) <- xs]

--showGame
--    Tjek at hver række er (w-1) + w * max{5,s} lang
--    Tjek at der er h rækker
prop_showGame = forAll anyLength $ \w ->
                forAll anyLength $ \h -> 
                sized $ \n -> let g = Game2048 w h in
                return $ forAll (anyValidMoveSequence g False (startState g) n) $ \xs ->
                and [ map (length) (lines $ showGame g s1) == replicate h (w-1+w*(max 5 (maxStringRepLength s1)))| (s1,np,nm,s2) <- xs]
                where maxStringRepLength s = maximum (map (length.show) (concat s))

return []
main = $quickCheckAll
