import Data.Tree
import Data.List hiding (insert)
import System.Environment (getArgs, getProgName)
import Data.Char


main :: IO ()
main = do
    args <- getArgs
    if (length args /= 3)
        then
            print "You need to input the three arguments: wordlist inputfile outputfile"
        else do
            wordlist <- readFile (args !! 0)-- "aspell-dump-expand/aspell-dump-expand-en.utf8.txt"
            text <- readFile (args !! 1) --"textfile.txt"
            writeFile (args !! 2) "" -- ^ empty outputfile
            correctify (trieify (words wordlist)) text "" (args !! 2)

-- * correct the text

-- | scan the text to find words correct them and write them into the output file
correctify :: Tree ([Char], Bool) -> [Char] -> [Char] -> [Char] -> IO ()
correctify wordtrie []     _ _          = print "ready"
correctify wordtrie (w:ws) p outputfile = 
                            if (not (isLetter w || w == '\'')) -- ^ words with ' are recognizeg too (e.g. ethnic's)
                                then do
                                    if (p /= []) 
                                        then
                                            do
                                                correct (sortedResults $ ldist (((('@',999),0):(initalRow 0 (zip ('@':(map toLower p)) (map toLower p)))),'@') wordtrie) p outputfile-- ^ only consider lower cased word for levenstein distance
                                                appendFile outputfile [w]
                                        else
                                            appendFile outputfile [w]
                                    correctify wordtrie ws [] outputfile
                                else
                                    correctify wordtrie ws (p++[w]) outputfile


-- | ask the user for correction of missspelled words
correct :: [([Char],Int)] -> [Char] -> [Char] -> IO ()
correct results p outputfile = 
        do
            if ((fst (results !! 0)) == p)
                then -- no error in word
                    appendFile outputfile p
                else do
                    putStrLn ("Mistake in " ++ p ++ " corrections:")
                    putStrLn (show (map fst $ take 5 results))
                    putStrLn "Enter 1-5 to take a suggestion 6 to display more results or enter the correct word manually."
                    w <- getLine
                    let n = (getNumber w 1 6) in 
                        if (n > 0 && n < 6)
                            then
                                appendFile outputfile (fst (results!!(n-1)))
                            else
                                if (n == 6)
                                    then 
                                        correct (drop 5 results) p outputfile
                                    else        
                                        appendFile outputfile w


-- | transforms a string of the number or into 21 if the string does not one of the numbers 1..20
getNumber :: [Char] -> Int -> Int -> Int
getNumber w nmin nmax = foldl (\num (n,strn) -> if (w == strn) then n else num) (nmax+1) (zip [nmin..nmax] (map show [nmin..nmax]))


-- * Levenstein distance
initalRow :: Int -> [(Char, Char)] -> [((Char, Int), Int)]
initalRow _     []            = []
initalRow below ((oldx,x):xs) = ((x,999),newbelow):(initalRow newbelow xs)
    where newbelow = (below + del oldx x)


-- | adds levestein distance to given word to nodes
ldist :: ([((Char,Int),Int)],Char) -> Tree ([Char],Bool) -> Tree ([Char],Bool,Int,Int)
ldist lcol (Node (p,f) ts) = Node (p,f,minimum $ map (\((c,x),y) -> min x y) $ fst nlcol,snd $ head $ reverse $ fst nlcol) (map (ldist nlcol) ts)
    where nlcol = foldl calcNewCol lcol (map toLower p) 


-- | calculate row with new distances
calcNewCol :: ([((Char,Int),Int)],Char) -> Char -> ([((Char,Int),Int)],Char)
calcNewCol (col@(((x,_),left):xs),oldy) y = (putSndCol (ncol) (999:999:(map snd col)),y) -- ^ memorize the last two columns, the second one pushed up by 2 rows since we have to look at the value 2 left and 2 below to calculate the costs for reverse
    where 
        newleft = left + ins x y 
        ncol = ((x,0),newleft):(traverseCol xs x y oldy left newleft)

        putSndCol :: [((Char,Int),Int)] -> [Int] -> [((Char,Int),Int)]
        putSndCol []             _      = []
        putSndCol (((x,_),d):xs) (y:ys) = ((x,y),d):(putSndCol xs ys)


-- | calculate the entries of the new column
traverseCol :: [((Char,Int),Int)] -> Char -> Char -> Char -> Int -> Int -> [((Char,Int),Int)]
traverseCol []                          _    _ _       _     _         = []
traverseCol (((x,bbelowlleft),left):xs) oldx y oldy belowleft below = ((x,0),newbelow):(traverseCol xs x y oldy left newbelow)
    where newbelow = minimum [belowleft + sub x y, left + ins x y, below + del oldy x, bbelowlleft + rev y oldy x oldx]


-- | cost for inserting y after x (leftarrow)
ins :: Char -> Char -> Int
ins x y = 2

-- | cost for deleting y after x (bottomarrow)
del :: Char -> Char -> Int
del x y = 2

-- | cost for substituting x by y (bottomleft)
sub :: Char -> Char -> Int
sub x y 
    |x == y = 0
    |True   = 2

-- | cost for reversing the order of the last two letters (bbelowlleft)
rev :: Char -> Char -> Char -> Char -> Int
rev y oldy x oldx 
    |y == oldx && x == oldy = 1
    |True                   = 999


-- | sortet list of best matches
sortedResults :: Tree ([Char],Bool,Int,Int) -> [([Char],Int)] 
sortedResults ts = concatMap (\d -> zip (wordsWithDist ts d) [d,d..]) [0..999]


-- | returns all words in trie with a certain distance
wordsWithDist :: Tree ([Char],Bool,Int,Int) -> Int -> [[Char]]
wordsWithDist (Node (p,f,mindist,dist) ts)  d
    |dist == d && f    = p:[p ++ w | w <- concatMap (\t -> wordsWithDist t d) ts]
    |mindist <= d      =   [p ++ w | w <- concatMap (\t -> wordsWithDist t d) ts]
    |True              = []


-- * create trie from words
trieify :: [[Char]] -> Tree ([Char],Bool)
trieify wordlist = foldl (\t w -> insertWordTree w t) (Node ("",False) []) wordlist


-- | insert a word into a trie
insertWordTree :: [Char] -> Tree ([Char],Bool) -> Tree ([Char],Bool)
-- can split Node into two Nodes
insertWordTree w (Node (praefix,final) ts) = ts `seq` (Node (newpraefix,newfinal) (insertWordList (movepraefix newp ts final) neww))
    where (newpraefix,newp,neww,newfinal) = getPreafix praefix w "" final


-- | checks how far the preafix matches the given word
getPreafix :: [Char] -> [Char] -> [Char] -> Bool -> ([Char],[Char],[Char],Bool)
getPreafix ps     []     l f = (l,ps,[],True)
getPreafix []     ws     l f = (l,[],ws,f)
getPreafix (p:ps) (w:ws) l f
    |p == w   = getPreafix ps ws (l++[p]) f
    |True     = (l,(p:ps),(w:ws),False) 


-- | push the preafix into the child nodes
movepraefix :: [Char] -> [Tree ([Char], Bool)] -> Bool -> [Tree ([Char], Bool)]
movepraefix "" ts                 _    = ts
movepraefix p  ts                 True = [(Node (p,True) ts)]
movepraefix p  []                 _    = error "leaf has to be True"
movepraefix p  [(Node (w,f) ts)]  _    = [(Node (p ++ w,f) ts)]
movepraefix p  ts                 _    = [(Node (p,False) ts)]


-- | insert a word into a list of subtries
insertWordList :: [Tree ([Char], Bool)] -> [Char] -> [Tree ([Char], Bool)]
insertWordList ts    [] = ts
insertWordList []     w = [(Node (w,True) [])]
insertWordList (t:ts) w
    |startswith t w     = (insertWordTree w t):ts
    |True               = t:(insertWordList ts w) 


-- | check whether this is the right trie to insert the word
startswith :: Eq a => Tree ([a], t) -> [a] -> Bool
startswith (Node ((p:ps),_) ts) (w:ws) = p == w


-- * usefull trie funtions

-- | test whether a given trie contains all words in a given list
check :: Tree ([Char], Bool) -> [[Char]] -> (Int, Int, Bool)
check trie wordlist = (length triewords,length $ listwords, triewords == listwords)
     where 
        triewords = sort $ collapse trie
        listwords = removeDubs [] (sort wordlist)


-- | remove dublicates from a sorted list
removeDubs :: Eq a => a -> [a] -> [a]
removeDubs _ [] = []
removeDubs w (x:xs)
    |x==w    = removeDubs w xs
    |True    = x:(removeDubs x xs)


-- | exract a list of all words in a trie
collapse :: Tree ([a], Bool) -> [[a]]
collapse (Node (p,f) ts)
    |f        = p:[p++w | w <- concatMap collapse ts] 
    |True     = [p++w | w <- concatMap collapse ts] 


-- | draw a trie into the console
drawTrie :: Show a => Tree a -> IO ()
drawTrie trie = putStrLn $ drawTree $ fmap show trie
