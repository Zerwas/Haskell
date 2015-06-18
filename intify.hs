import Prelude hiding (mapM)

import Control.DeepSeq
import Control.Monad.State.Lazy hiding (mapM)
import Data.List (foldl')
import Data.Traversable (mapM)
import Data.Tree
import Data.Tuple (swap)
import System.Environment (getArgs, getProgName)
import System.Random
import Data.List


intify::Eq a => [Tree a] -> ([Tree Int], [(a,Int)])
intify xs = let ms = evalState (extend xs) [] in (map (mapp ms) xs, ms)

extend::Eq a => [Tree a] -> State ([(a,Int)])  [(a,Int)]
extend [] = do
  ms <- get
  return ms
extend ((Node e ts):tss) = do
  ms <- get
  put (ins e 0 ms)
  extend (ts++tss)

ins::Eq a => a -> Int -> [(a,Int)] -> [(a,Int)]
ins a n []         = [(a,n)]
ins a n ((x,m):xs)
  |a==x            = (x,m):xs
  |True            = (x,m):(ins a (n+1) xs)

mapp::Eq a => [(a,Int)] -> Tree a -> Tree Int
mapp ms (Node e ts) = Node (get e ms) (map (mapp ms) ts)
    where
      get e ((x,n):xs)
        |e==x      = n
        |True      = get e xs

{-intify::Eq a=>[Tree a] -> ([Tree Int], [(a,Int)])
intify xs = (map f xs,ls) 
    where ls = zip (nub (conc (map flatten xs) [] )) [0..]
          f (Node e ts) = Node (get e ls) (map f ts)
              where get e ((x,n):xs)
                       |e==x      = n
                       |True      = get e xs -}
-- Tests ---------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["correctness"] -> do
      putStr "Testing []           ... "
      testCorrectness ([] :: [Tree ()])
      putStr "Testing random trees ... "
      testCorrectness $ genTreesRnd 0 ('a', 'z') 9 1000
    ["heap0"]  -> testHeap  $ genTrees 1000
    ["heap1"]  -> testHeap  $ genTreesRnd 0 ('a', 'z') 9 20000
    ["stack0"] -> testStack $ genTrees 500
    ["stack1"] -> testStack $ genTreesRnd 0 ('a', 'z') 9 20000
    _ -> printHelp


printHelp :: IO ()
printHelp = getProgName >>= \ progName -> mapM_ putStrLn
  [ progName ++ " <CMD>"
  , "  where <CMD> is one of"
  , "    correctness   : Do a simple correctness test."
  , "    heap0, heap1  : These shall not consume much memory, otherwise you"
  , "                    are too lazy. Try with +RTS -M32M -RTS."
  , "    stack0, stack1: These shall not lead to stack space overflows."
  ]


testCorrectness :: Eq a => [Tree a] -> IO ()
testCorrectness ts = do
  let (ts', assocs) = intify ts
  case mapM (mapM (flip lookup (map swap assocs))) ts' of
    Nothing -> putStrLn "Error: The returned association list is incomplete."
    Just ts'' | ts /= ts'' -> putStrLn "Error: The returned trees or \
                                       \association list are wrong."
              | otherwise  -> putStrLn "Test passed."


testHeap :: (NFData a, Eq a) => [Tree a] -> IO ()
testHeap
  = print
  . foldl' (+) 0
  . concatMap flatten
  . fst
  . intify
  . listDeepseq


testStack :: (NFData a, Eq a) => [Tree a] -> IO ()
testStack
  = print
  . last
  . concatMap flatten
  . fst
  . intify
  . listDeepseq


genTrees :: Int -> [Tree Int]
genTrees cnt
  = unfoldForest
      ( \ n ->
        ( n
        , if n > 1 then let n2 = n `div` 2 in [n2, n - n2] else []
        )
      )
      [1 :: Int .. cnt]
  ++ [Node 99999999 []]


genTreesRnd :: Random a => Int -> (a, a) -> Int -> Int -> [Tree a]
genTreesRnd seed bounds maxChildren cnt
  = evalState
      (replicateM cnt (genTreeRnd bounds maxChildren))
      (mkStdGen seed)


genTreeRnd :: (Random a, RandomGen g) => (a, a) -> Int -> State g (Tree a)
genTreeRnd bounds c = do
  r <- state $ randomR bounds
  c' <- state $ randomR (0, c - 1)
  ts <- replicateM c' $ genTreeRnd bounds c'
  return (Node r ts)


listDeepseq :: NFData a => [a] -> [a]
listDeepseq = foldr (\ x xs -> x `deepseq` (x : xs)) []

