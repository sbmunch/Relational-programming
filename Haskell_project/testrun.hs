import Game
import Game2048
import GameStrategies

import System.Random
import System.Environment

import Control.Monad
import Text.Read


playMinimax g (p,s) d = handleMove g p s (fst $ minimax g (takeTree d (tree g (p,s)))) d playMinimax
playExpectimax g (p@True,s) d = handleMove g p s (fst $ expectimax g (takeTree d (tree g (p,s)))) d playExpectimax
playExpectimax g (p@False,s) d = do
    putStrLn $ showGame g s
    gen <- newStdGen
    let r = fst (randomR (0,1) gen) :: Double
    let m = head [ m | (r',m) <- cumdist ms, r' < r]
    putStrLn $ "Player " ++ (show p) ++ " performs move " ++ (show m)
    playExpectimax g (not p, move g p s m) d
    where 
      cumdist = snd . foldr (\(x,y) (a,xs) -> (a+x,(a,y):xs)) (0,[])
      ms = (moves g False s)
      
handleMove g p s m d rec = do
  putStrLn $ showGame g s
  case m of
    Nothing -> do
      putStrLn $ "No available moves!"
    Just m -> do
      putStrLn $ "Player " ++ (show p) ++ " performs move " ++ (show m)
      rec g (not p, move g p s m) d

data Algorithm = Minimax | Expectimax

parseArgs :: [String] -> Maybe (Algorithm, Game2048, Depth)
parseArgs xs = do
  guard $ length xs == 4
  let [alg,width,height,depth] = xs
  a <- case alg of
    "minimax"     -> Just Minimax
    "expectimax"  -> Just Expectimax
    _             -> Nothing
  w <- readMaybe width 
  h <- readMaybe height
  d <- readMaybe depth
  return (a,Game2048 w h, d)

main = do
  xs <- getArgs
  case (parseArgs xs) of
    Just (Minimax,g,d)      -> playMinimax g (False,startState g) d
    Just (Expectimax,g,d)   -> playExpectimax g (False,startState g) d
    Nothing -> putStrLn desc
  where 
    desc = "Usage: ./testrun [minimax|expectimax] [board width] [board height] [tree depth]"
