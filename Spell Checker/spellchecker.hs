import Control.Monad.State.Lazy
import Data.Tree
import Data.List

main = do
    wordlist <- readFile "aspell-dump-expand/aspell-dump-expand-de_DE.utf8.txt"
    text <- readFile "textfile.txt"
    return (check (trieify (words wordlist)) (words wordlist))--(correct (trieify (words wordlist)) text)

-- * correct the text
correct wordlist text = drawTree $ mapTree show wordlist


-- * Levenstein distance
-- | adds levestein distance to given word to nodes
ldist :: [Char] -> Tree ([Char],Bool) -> Tree ([Char],Bool,Int)
ldist w (Node (p,f) ts)
    |p == w    = Node (p,f,0) (map (ldist w) ts)
    |True      = Node (p,f,1) (map (ldist w) ts)


-- | sortet list of best matches
sortedResults :: Tree ([Char],Bool,Int) -> [([Char],Int)] 
sortedResults ts = concatMap (\d -> zip (wordsWithDist ts d) [d,d..]) [0..]


-- | returns all words in trie with a certain distance
wordsWithDist :: Tree ([Char],Bool,Int) -> Int -> [[Char]]
wordsWithDist (Node (p,f,dist) ts) d
    |dist == d && f    = p:[p++w | w <- concatMap (\t -> wordsWithDist t d) ts]
    |dist <= d         =   [p++w | w <- concatMap (\t -> wordsWithDist t d) ts]
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

check trie wordlist = (length triewords,length $ (removeDubs "" (sort wordlist)))
     where triewords = collapse trie

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