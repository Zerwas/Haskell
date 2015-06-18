module Main where
import Control.Monad.State

import System.Environment (getArgs)


data Edit a = Deletion a | Insertion a | Substitution a deriving Show


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file1, file2] -> mainDiff file1 file2
    _ -> putStrLn "Expecting exactly two filenames as command line arguments."


mainDiff :: FilePath -> FilePath -> IO ()
mainDiff file1 file2 = do
  contents1 <- readFile file1
  contents2 <- readFile file2
  putStr $ unlines $ map draw $ snd $ diff (lines contents1) (lines contents2)
  where
    draw (Deletion     x) = "< " ++ x
    draw (Insertion    x) = "> " ++ x
    draw (Substitution x) = "= " ++ x



diff :: Eq a => [a] -> [a] -> (Int, [Edit a])
diff xs ys = evalState (med xs ys) ((initE ys []),[0..])

initE [] es     = [es]
initE (y:ys) es = es:(initE ys (es++[Insertion y]))

med :: Eq a => [a] -> [a] -> State ([[Edit a]],[Int]) (Int, [Edit a])
med [] _      = do
  (edit,ls) <- get
  return (head$reverse ls,head$reverse edit)
med (x:xs) ys = do
  (edit,ls) <- get
  put (update x ys edit ls)
  med xs ys

--update line of table
update :: Eq a => a -> [a] -> [[Edit a]] -> [Int] -> ([[Edit a]],[Int])
update x ys (e:edits) (l:ls) = evalState (stat x ys edits ls) (l+1,l,e++[Deletion x],e,[l+1],[e++[Deletion x]])

--State(left l, leftbelow l, left e, leftbelow e, new line, new edits)
stat :: Eq a => a -> [a] -> [[Edit a]] -> [Int] -> State (Int,Int,[Edit a],[Edit a],[Int],[[Edit a]]) ([[Edit a]],[Int])
stat x [] _ _ = do
  (_,_,_,_,ls,edit) <- get
  return (edit, ls)
stat x (y:ys) (e:es) (l:ls) = do
  (newl,oldl,newe,olde,ls',edit) <- get
  let (nl,ne) = f x y newl newe oldl olde l e in put (nl,l,ne,e,ls'++[nl],edit++[ne])
  stat x ys es ls

f x y newl newe oldl olde l e
  |x==y   = (oldl,olde++[Substitution y])
  |newl>l = (l+1,e++[Deletion x])
  |True   = (newl+1,newe++[Insertion y])