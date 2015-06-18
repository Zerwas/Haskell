import Control.Monad.State.Lazy
import Data.Tree

main = do
    words <- readFile "aspell-dump-expand/aspell-dump-expand-de_DE.utf8.txt"
    text <- readFile "textfile"
    return (correct (trieify (toLines words "")) text)

correct words text = undefined


-- * create trie from words
trieify :: [[Char]] -> Tree ([Char],Bool)
trieify words = foldr insertWordTree (Node ("",False) []) words


-- | insert a word into a trie
insertWordTree :: [Char] -> Tree ([Char],Bool) -> Tree ([Char],Bool)
-- can split Node into two Nodes
insertWordTree w (Node (praefix,final) ts)   =  (Node (newpraefix,newfinal) (insertWordList (insertWordList ts neww) newp))
    where (newpraefix,newp,neww,newfinal) = getPreafix praefix w "" final


-- | checks how far the preafix matches the given word
getPreafix :: [Char] -> [Char] -> [Char] -> Bool -> ([Char],[Char],[Char],Bool)
getPreafix ps     []     l f = (l,ps,[],True)
getPreafix []     ws     l f = (l,[],ws,f)
getPreafix (p:ps) (w:ws) l f
    |p==w     = getPreafix ps ws (l++[p]) f
    |True     = (l,(p:ps),(w:ws),False) 

insertWordList ts    [] = ts
insertWordList []     w = [(Node (w,True) [])]
insertWordList (t:ts) w
    |startswith t w     = (insertWordTree w t):ts
    |True               = t:(insertWordList ts w) 

startswith (Node ((p:ps),_) ts) (w:ws) = p==w


-- | convert char array to list of words
toLines :: [Char] -> [Char] -> [[Char]]
toLines [] w = [reverse w]
toLines (x:xs) w
    |x=='\n' = [reverse w] ++ (toLines xs "")
    |True    = toLines xs (x:w)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node n ts) = (Node (f n) (map (mapTree f) ts))


drawTrie :: Show a => Tree a -> IO ()
drawTrie trie = putStrLn $ drawTree $ mapTree show trie