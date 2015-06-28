import Control.Monad.State.Lazy
import Data.Tree
import Data.List
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    args <- getArgs
    wordlist <- readFile "aspell-dump-expand/aspell-dump-expand-de_DE.utf8.txt" --(args !! 0)
    text <- readFile "textfile.txt" --(args !! 1)
    writeFile "newfile.txt" "" -- ^ empty outputfile
    correctify (trieify (words wordlist)) text ""
    --(take 40 (sortedResults $ ldist (zip " Aachen" [0..]) (trieify (words wordlist))))
    --(check (trieify (words wordlist)) (words wordlist))

-- * correct the text
correctify :: Tree ([Char], Bool) -> [Char] -> [Char] -> IO ()
correctify wordtrie []     _ = print "ready"
correctify wordtrie (w:ws) p = 
                            if (any (w ==) ['\n',' ',',','.',':','!','?',';'])
                                then do
                                    when (p /= []) $
                                        correct (map fst (sortedResults $ ldist (zip (' ':p) [0..]) wordtrie)) p
                                    appendFile "newfile.txt" [w]
                                    correctify wordtrie ws []
                                else
                                    correctify wordtrie ws (p++[w])

correct :: [[Char]] -> [Char] -> IO ()
correct results p = 
        do
            if ((results !! 0) == p)
                then -- no error in word
                    appendFile "newfile.txt" p
                else do
                    putStrLn ("Mistake in " ++ p ++ " corrections:")
                    putStrLn (show (take 5 results))
                    putStrLn "Enter 1-5 to take a suggestion 6 to display more results or enter the correct word manually."
                    w <- getLine
                    let n = (getNumber w) in 
                        if (n > 0 && n < 6)
                            then
                                appendFile "newfile.txt" (results!!(getNumber w))
                            else
                                if (n == 6)
                                    then 
                                        correct (drop 5 results) p
                                    else        
                                        appendFile "newfile.txt" w


-- | transforms a string of the number or into 21 if the string does not one of the numbers 1..20
getNumber :: [Char] -> Int
getNumber w = foldl (\num (n,strn) -> if (w == strn) then n else num) 21 (zip [1..20] (map show [1..20]))


-- * Levenstein distance
-- | adds levestein distance to given word to nodes
ldist :: [(Char,Int)] -> Tree ([Char],Bool) -> Tree ([Char],Bool,Int,Int)
ldist lrow (Node (p,f) ts) = Node (p,f,minimum $ map snd nlrow,snd $ head $ reverse nlrow) (map (ldist nlrow) ts)
    where nlrow = foldl calcNewRow lrow p 


-- | calculate row with new distances
calcNewRow :: [(Char,Int)] -> Char -> [(Char,Int)]
calcNewRow ((x,left):xs) y = (x,left+1):(traverseRow xs y left (left+1))


traverseRow :: [(Char,Int)] -> Char -> Int -> Int -> [(Char,Int)]
traverseRow []            _ _     _         = []
traverseRow ((x,left):xs) y belowleft below = (x,newbelow):(traverseRow xs y left newbelow)
    where newbelow = min (belowleft + dist x y) ((min left below)+1)


-- | cost for substituting y by x
dist :: Char -> Char -> Int
dist x y 
    |x==y = 0
    |True = 1


-- | sortet list of best matches
sortedResults :: Tree ([Char],Bool,Int,Int) -> [([Char],Int)] 
sortedResults ts = concatMap (\d -> zip (wordsWithDist ts d) [d,d..]) [0..100]


-- | returns all words in trie with a certain distance
wordsWithDist :: Tree ([Char],Bool,Int,Int) -> Int -> [[Char]]
wordsWithDist (Node (p,f,mindist,dist) ts)  d
    |dist == d && f    = p:[p++w | w <- concatMap (\t -> wordsWithDist t d) ts]
    |mindist <= d      =   [p++w | w <- concatMap (\t -> wordsWithDist t d) ts]
    |True              = []


-- * create trie from words
trieify :: [[Char]] -> Tree ([Char],Bool)
trieify wordlist = foldr insertWordTree (Node ("",False) []) wordlist


-- | insert a word into a trie
insertWordTree :: [Char] -> Tree ([Char],Bool) -> Tree ([Char],Bool)
-- can split Node into two Nodes
insertWordTree w (Node (praefix,final) ts) = (Node (newpraefix,newfinal) (insertWordList (movepraefix newp ts final) neww))
    where (newpraefix,newp,neww,newfinal) = getPreafix praefix w "" final


-- | checks how far the preafix matches the given word
getPreafix :: [Char] -> [Char] -> [Char] -> Bool -> ([Char],[Char],[Char],Bool)
getPreafix ps     []     l f = (l,ps,[],True)
getPreafix []     ws     l f = (l,[],ws,f)
getPreafix (p:ps) (w:ws) l f
    |p==w     = getPreafix ps ws (l++[p]) f
    |True     = (l,(p:ps),(w:ws),False) 


-- | push the preafix into the child nodes
movepraefix :: [Char] -> [Tree ([Char], Bool)] -> Bool -> [Tree ([Char], Bool)]
movepraefix "" ts                 _    = ts
movepraefix p  ts                 True = [(Node (p,True) ts)]
movepraefix p  []                 _    = error "leaf has to be True"
movepraefix p  [(Node (w,f) ts)]  _    = [(Node (p++w,f) ts)]
movepraefix p  ts                 _    = [(Node (p,False) ts)]


insertWordList :: [Tree ([Char], Bool)] -> [Char] -> [Tree ([Char], Bool)]
insertWordList ts    [] = ts
insertWordList []     w = [(Node (w,True) [])]
insertWordList (t:ts) w
    |startswith t w     = (insertWordTree w t):ts
    |True               = t:(insertWordList ts w) 


startswith :: Eq a => Tree ([a], t) -> [a] -> Bool
startswith (Node ((p:ps),_) ts) (w:ws) = p==w


-- TODO delete
-- | convert char array to list of words
toLines :: [Char] -> [Char] -> [[Char]]
toLines [] w = [reverse w]
toLines (x:xs) w
    |x=='\n' = [reverse w] ++ (toLines xs "")
    |True    = toLines xs (x:w)


-- * usefull trie funtions

check trie wordlist = (length triewords,length $ listwords, triewords == listwords)
     where 
        triewords = sort $ collapse trie
        listwords = removeDubs "" (sort wordlist)


removeDubs _ [] = []
removeDubs w (x:xs)
    |x==w    = removeDubs w xs
    |True    = x:(removeDubs x xs)


collapse (Node (p,f) ts)
    |f        = p:[p++w | w <- concatMap collapse ts] 
    |True     = [p++w | w <- concatMap collapse ts] 


sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _      = True
sublist (x:xs) ys = (foldr (\y f -> f || (x == y)) False ys) && (sublist xs ys)


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node n ts) = (Node (f n) (map (mapTree f) ts))


drawTrie :: Show a => Tree a -> IO ()
drawTrie trie = putStrLn $ drawTree $ mapTree show trie