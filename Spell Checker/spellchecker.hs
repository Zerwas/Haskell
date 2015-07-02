import Control.Monad.State.Lazy
import Data.Tree
import Data.List hiding (insert)
import System.Environment (getArgs, getProgName)
import Data.Char
import Data.Map hiding (foldr,foldl,map)

main :: IO ()
main = do
    args <- getArgs
    wordlist <- readFile "aspell-dump-expand/aspell-dump-expand-en.utf8.txt" --(args !! 0)
    text <- readFile "textfile.txt" --(args !! 1)
    writeFile "newfile.txt" "" -- ^ empty outputfile
    correctify (trieify (words wordlist)) text ""
    --(take 40 (sortedResults $ ldist (zip " Aachen" [0..]) (trieify (words wordlist))))
    --(check (trieify (words wordlist)) (words wordlist))

-- * correct the text
correctify :: Tree ([Char], Bool) -> [Char] -> [Char] -> IO ()
correctify wordtrie []     _ = print "ready"
correctify wordtrie (w:ws) p = 
                            if (not (isLetter w || w == '\'')) -- ^ words with ' are recognizeg too (e.g. ethnic's)
                                then do
                                    when (p /= []) $
                                        correct (sortedResults $ ldist (('@',0):(initalRow 0 (zip ('@':(map toLower p)) (map toLower p)))) wordtrie) p -- ^ only consider lower cased word for levenstein distance
                                    appendFile "newfile.txt" [w]
                                    correctify wordtrie ws []
                                else
                                    correctify wordtrie ws (p++[w])

correct :: [([Char],Int)] -> [Char] -> IO ()
correct results p = 
        do
            if ((fst (results !! 0)) == p)
                then -- no error in word
                    appendFile "newfile.txt" p
                else do
                    putStrLn ("Mistake in " ++ p ++ " corrections:")
                    putStrLn (show (take 5 results))
                    putStrLn "Enter 1-5 to take a suggestion 6 to display more results or enter the correct word manually."
                    w <- getLine
                    let n = (getNumber w 1 6) in 
                        if (n > 0 && n < 6)
                            then
                                appendFile "newfile.txt" (fst (results!!n))
                            else
                                if (n == 6)
                                    then 
                                        correct (drop 5 results) p
                                    else        
                                        appendFile "newfile.txt" w


-- | transforms a string of the number or into 21 if the string does not one of the numbers 1..20
getNumber :: [Char] -> Int -> Int -> Int
getNumber w nmin nmax = foldl (\num (n,strn) -> if (w == strn) then n else num) (nmax+1) (zip [nmin..nmax] (map show [nmin..nmax]))


-- * Levenstein distance
initalRow :: Int -> [(Char, Char)] -> [(Char, Int)]
initalRow _     []            = []
initalRow below ((oldx,x):xs) = (x,newbelow):(initalRow newbelow xs)
    where newbelow = (below + del oldx x)


-- | adds levestein distance to given word to nodes
ldist :: [(Char,Int)] -> Tree ([Char],Bool) -> Tree ([Char],Bool,Int,Int)
ldist lrow (Node (p,f) ts) = Node (p,f,minimum $ map snd nlrow,snd $ head $ reverse nlrow) (map (ldist nlrow) ts)
    where nlrow = foldl calcNewRow lrow (map toLower p) 


-- | calculate row with new distances
calcNewRow :: [(Char,Int)] -> Char -> [(Char,Int)]
calcNewRow ((x,left):xs) y = (x,newleft):(traverseRow xs x y left newleft)
    where newleft = left + ins x y 


traverseRow :: [(Char,Int)] -> Char -> Char -> Int -> Int -> [(Char,Int)]
traverseRow []            _    _ _     _         = []
traverseRow ((x,left):xs) oldx y belowleft below = (x,newbelow):(traverseRow xs x y left newbelow)
    where newbelow = min (belowleft + sub x y) (min (left + ins x y) (below + del oldx x))


-- | cost for inserting y after x (leftarrow)
ins :: Char -> Char -> Int
--ins x y = 1
ins = curry (\x -> findWithDefault 999 x (fromMatrix $ map words insertMatrix))

-- | cost for deleting y after x (bottomarrow)
del :: Char -> Char -> Int
--del x y = 1
del = curry (\x -> findWithDefault 999 x (fromMatrix $ map words deletionMatrix))

-- | cost for substituting x by y (bottomleft)
sub :: Char -> Char -> Int
sub x y 
    |x==y = 0
    |True = curry (\x -> findWithDefault 999 x (fromMatrix $ map words substitutionMatrix)) x y
--sub = curry (\x -> findWithDefault 999 x (fromMatrix $ map words substitutionMatrix))


-- | sortet list of best matches
sortedResults :: Tree ([Char],Bool,Int,Int) -> [([Char],Int)] 
sortedResults ts = concatMap (\d -> zip (wordsWithDist ts d) [d,d..]) [0..100]


-- | returns all words in trie with a certain distance
wordsWithDist :: Tree ([Char],Bool,Int,Int) -> Int -> [[Char]]
wordsWithDist (Node (p,f,mindist,dist) ts)  d
    |dist == d && f    = p:[p++w | w <- concatMap (\t -> wordsWithDist t d) ts]
    |mindist <= d      =   [p++w | w <- concatMap (\t -> wordsWithDist t d) ts]
    |True              = []


-- ^ creates mapping from Matrix in string form
fromMatrix :: [[[Char]]] -> Map (Char,Char) Int
fromMatrix (x:ys) = foldl (fromRow x) empty ys
    where
        fromRow x mapping ((y:[]):ys) = foldl (\mapp ((x:[]),n) -> insert (y,x) (value n) mapp) mapping (zip x ys) 
            where value n = - fromInteger $ fromIntegral (getNumber n 0 999)/999


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

-- * Confusion matrices

insertMatrix =         ["    a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z",
                        "a  15   1  14   7  10   0   1   1  33   1   4  31   2  39  12   4   3  28 134   7  28   0   1   1   4   1",
                        "b   3  11   0   0   7   0   1   0  50   0   0  15   0   1   1   0   0   5  16   0   0   3   0   0   0   0",
                        "c  19   0  54   1  13   0   0  18  50   0   3   1   1   1   7   1   0   7  25   7   8   4   0   1   0   0",
                        "d  18   0   3  17  14   2   0   0   9   0   0   6   1   9  13   0   0   6 119   0   0   0   0   0   5   0",
                        "e  39   2   8  76 147   2   0   1   4   0   3   4   6  27   5   1   0  83 417   6   4   1  10   2   8   0",
                        "f   1   0   0   0   2  27   1   0  12   0   0  10   0   0   0   0   0   5  23   0   1   0   0   0   1   0",
                        "g   8   0   0   0   5   1   5  12   8   0   0   2   0   1   1   0   1   5  69   2   3   0   1   0   0   0",
                        "h   4   1   0   1  24   0  10  18  17   2   0   1   0   1   4   0   0  16  24  22   1   0   5   0   3   0",
                        "i  10   3  13  13  25   0   1   1  69   2   1  17  11  33  27   1   0   9  30  29  11   0   0   1   0   1",
                        "j   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0   0   0   0",
                        "k   2   4   0   1   9   0   0   1   1   0   1   1   0   0   2   1   0   0  95   0   1   0   0   0   4   0",
                        "l   3   1   0   1  38   0   0   0  79   0   2 128   1   0   7   0   0   0  97   7   3   1   0   0   2   0",
                        "m  11   1   1   0  17   0   0   1   6   0   1   0 102  44   7   2   0   0  47   1   2   0   1   0   0   0",
                        "n  15   5   7  13  52   4  17   0  34   0   1   1  26  99  12   0   0   2 156  53   1   1   0   0   1   0",
                        "o  14   1   1   3   7   2   1   0  28   1   0   6   3  13  64  30   0  16  59   4  19   1   0   0   1   1",
                        "p  23   0   1   1  10   0   0  20   3   0   0   2   0   0  26  70   0  29  52   9   1   1   1   0   0   0",
                        "q   0   0   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   1   0   0   0   0   0",
                        "r  15   2   1   0  89   1   1   2  64   0   0   5   9   7  10   0   0 132 273  29   7   0   1   0  10   0",
                        "s  13   1   7  20  41   0   1  50 101   0   2   2  10   7   3   1   0   1 205  49   7   0   1   0   7   0",
                        "t  39   0   0   3  65   1  10  24  59   1   0   6   3   1  23   1   0  54 264 183  11   0   5   0   6   0",
                        "u  15   0   3   0   9   0   0   1  24   1   1   3   3   9   1   3   0  49  19  27  26   0   0   2   3   0",
                        "v   0   2   0   0  36   0   0   0  10   0   0   1   0   1   0   1   0   0   0   0   1   5   1   0   0   0",
                        "w   0   0   0   1  10   0   0   1   1   0   1   1   0   2   0   0   1   1   8   0   2   0   4   0   0   0",
                        "x   0   0  18   0   1   0   0   6   1   0   0   0   1   0   3   0   0   0   2   0   0   0   0   1   0   0",
                        "y   5   1   2   0   3   0   0   0   2   0   0   1   1   6   0   0   0   1  33   1  13   0   1   0   2   0",
                        "z   2   0   0   0   5   1   0   0   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   2   4",
                        "@  46   8   9   8  26  11  14   3   5   1  17   5   6   2   2  10   0   6  23   2  11   1   2   1   1   2"]

deletionMatrix =       ["    a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z",
                        "a   0   7  58  21   3   5  18   8  61   0   4  43   5  53   0   9   0  98  28  53  62   1   0   0   2   0",
                        "b   2   2   1   0  22   0   0   0 183   0   0  26   0   0   2   0   0   6  17   0   6   1   0   0   0   0",
                        "c  37   0  70   0  63   0   0  24 320   0   9  17   0   0  33   0   0  46   6  54  17   0   0   0   1   0",
                        "d  12   0   7  25  45   0  10   0  62   1   1   8   4   3   3   0   0  11   1   0   3   2   0   0   6   0",
                        "e  80   1  50  74  89   3   1   1   6   0   0  32   9  76  19   9   1 237 223  34   8   2   1   7   1   0",
                        "f   4   0   0   0  13  46   0   0  79   0   0  12   0   0   4   0   0  11   0   8   1   0   0   0   1   0",
                        "g  25   0   0   2  83   1  37  25  39   0   0   3   0  29   4   0   0  52   7   1  22   0   0   0   1   0",
                        "h  15  12   1   3  20   0   0  25  24   0   0   7   1   9  22   0   0  15   1  26   0   0   1   0   1   0",
                        "i  26   1  60  26  23   1   9   0   1   0   0  38  14  82  41   7   0  16  71  64   1   1   0   0   1   7",
                        "j   0   0   0   0   1   0   0   0   0   0   0   0   0   1   1   0   0   0   0   0   1   0   0   0   0   0",
                        "k   4   0   0   1  15   1   8   1   5   0   1   3   0  17   0   0   0   1   5   0   0   0   1   0   0   0",
                        "l  24   0   1   6  48   0   0   0 217   0   0 211   2   0  29   0   0   2  12   7   3   2   0   0  11   0",
                        "m  15  10   0   0  33   0   0   1  42   0   0   0 180   7   7  31   0   0   9   0   4   0   0   0   0   0",
                        "n  21   0  42  71  68   1 160   0 191   0   0   0  17 144  21   0   0   0 127  87  43   1   1   0   2   0",
                        "o  11   4   3   6   8   0   5   0   4   1   0  13   9  70  26  20   0  98  20  13  47   2   5   0   1   0",
                        "p  25   0   0   0  22   0   0  12  15   0   0  28   1   0  30  93   0  58   1  18   2   0   0   0   0   0",
                        "q   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  18   0   0   0   0   0",
                        "r  63   4  12  19 188   0  11   5 132   0   3  33   7 157  21   2   0 277 103  68   0  10   1   0  27   0",
                        "s  16   0  27   0  74   1   0  18 231   0   0   2   1   0  30  30   0   4 265 124  21   0   0   0   1   0",
                        "t  24   1   2   0  76   1   7  49 427   0   0  31   3   3  11   1   0 203   5 137  14   0   4   0   2   0",
                        "u  26   6   9  10  15   0   1   0  28   0   0  39   2 111   1   0   0 129  31  66   0   0   0   0   1   0",
                        "v   9   0   0   0  58   0   0   0  31   0   0   0   0   0   2   0   0   1   0   0   0   0   0   0   1   0",
                        "w  40   0   0   1  11   1   0  11  15   0   0   1   0   2   2   0   0   2  24   0   0   0   0   0   0   0",
                        "x   1   0  17   0   3   0   0   1   0   0   0   0   0   0   0   6   0   0   0   5   0   0   0   0   1   0",
                        "y   2   1  34   0   2   0   1   0   1   0   0   1   2   1   1   1   0   0  17   1   0   0   1   0   0   0",
                        "z   1   0   0   0   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   2",
                        "@  20  14  41  31  20  20   7   6  20   3   6  22  16   5   5  17   0  28  26   6   2   1  24   0   0   2"]

substitutionMatrix =   ["    a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z",
                        "a   0   0   7   1 342   0   0   2 118   0   1   0   0   3  76   0   0   1  35   9   9   0   1   0   5   0",
                        "b   0   0   9   9   2   2   3   1   0   0   0   5  11   5   0  10   0   0   2   1   0   0   8   0   0   0",
                        "c   6   5   0  16   0   9   5   0   0   0   1   0   7   9   1  10   2   5  39  40   1   3   7   1   1   0",
                        "d   1  10  13   0  12   0   5   5   0   0   2   3   7   3   0   1   0  43  30  22   0   0   4   0   2   0",
                        "e 388   0   3  11   0   2   2   0  89   0   0   3   0   5  93   0   0  14  12   6  15   0   1   0  18   0",
                        "f   0  15   0   3   1   0   5   2   0   0   0   3   4   1   0   0   0   6   4  12   0   0   2   0   0   0",
                        "g   4   1  11  11   9   2   0   0   0   1   1   3   0   0   2   1   3   5  13  21   0   0   1   0   3   0",
                        "h   1   8   0   3   0   0   0   0   0   0   2   0  12  14   2   3   0   3   1  11   0   0   2   0   0   0",
                        "i 103   0   0   0 146   0   1   0   0   0   0   6   0   0  49   0   0   0   2   1  47   0   2   1  15   0",
                        "j   0   1   1   9   0   0   1   0   0   0   0   2   1   0   0   0   0   0   5   0   0   0   0   0   0   0",
                        "k   1   2   8   4   1   1   2   5   0   0   0   0   5   0   2   0   0   0   6   0   0   0   4   0   0   3",
                        "l   2  10   1   4   0   4   5   6  13   0   1   0   0  14   2   5   0  11  10   2   0   0   0   0   0   0",
                        "m   1   3   7   8   0   2   0   6   0   0   4   4   0 180   0   6   0   0   9  15  13   3   2   2   3   0",
                        "n   2   7   6   5   3   0   1  19   1   0   4  35  78   0   0   7   0  28   5   7   0   0   1   2   0   2",
                        "o  91   1   1   3 116   0   0   0  25   0   2   0   0   0   0  14   0   2   4  14  39   0   0   0  18   0",
                        "p   0  11   1   2   0   6   5   0   2   9   0   2   7   6  15   0   0   1   3   6   0   4   1   0   0   0",
                        "q   0   0   1   0   0   0  27   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0",
                        "r   0  14   0  30  12   2   2   8   2   0   5   8   4  20   1  14   0   0  12  22   4   0   0   1   0   0",
                        "s  11   8  27  33  35   4   0   1   0   1   0  27   0   6   1   7   0  14   0  15   0   0   5   3  20   1",
                        "t   3   4   9  42   7   5  19   5   0   1   0  14   9   5   5   6   0  11  37   0   0   2  19   0   7   6",
                        "u  20   0   0   0  44   0   0   0  64   0   0   0   0   2  43   0   0   4   0   0   0   0   2   0   8   0",
                        "v   0   0   7   0   0   3   0   0   0   0   0   1   0   0   1   0   0   0   8   3   0   0   0   0   0   0",
                        "w   2   2   1   0   1   0   0   2   0   0   1   0   0   0   0   7   0   6   3   3   1   0   0   0   0   0",
                        "x   0   0   0   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   9   0   0   0   0   0   0   0",
                        "y   0   0   2   0  15   0   1   7  15   0   0   0   2   0   6   1   0   7  36   8   5   0   0   1   0   0",
                        "z   0   0   0   7   0   0   0   0   0   0   0   7   5   0   0   0   0   2  21   3   0   0   0   0   3   0"]