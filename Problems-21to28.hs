import Data.Array
import Data.List (sortBy,groupBy)
import System.Random
import Control.Monad (replicateM)

-- 21) Insert an element at a given position into a list.
insertAt e xs n = take (n-1) xs ++ [e] ++ drop (n-1) xs 

-- 22) Create a list containing all integers within a given range.
range m n = [m..n]

-- 23) Extract a given number of randomly selected elements from a list.

rnd_select xs n 
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return (xs!!r)

-- 24) Lotto: Draw N different random numbers from the set 1..M.                               
diff_select n m = rnd_select [1..m] n 

-- 25) Generate a random permutation of the elements of a list.

arraySwap :: Ix i => Array i e -> i -> i -> Array i e
arraySwap arr i j = let i' = arr ! i
                    in arr // [(i, arr ! j), (j, i')]

arrayShuffle arr [] = arr
arrayShuffle arr ((i,j):xs) = arrayShuffle (arraySwap arr i j) xs

rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu (x:xs) = do
  rand <- randomRIO (0, length xs)
  rest <- rnd_permu xs
  return $ let (ys,zs) = splitAt rand rest
               in ys++(x:zs) 

arrayRandomShuffle :: (Enum i, Ix i) => Array i e -> IO (Array i e)
arrayRandomShuffle arr = do
                            xs <- rnd_permu [m..n]
                            return $ arrayShuffle arr (zip [m..n] xs)
                          where (m,n) = bounds arr

--  Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements of a list 

combinations 0 _ = [[]]
combinations _ [] = []
combinations n ys@(x:xs)
  | n < 0 = []
  | otherwise = case drop (n-1) ys of
    [] -> []
    [_] -> [ys]
    _ -> [x:c | c <- combinations (n-1) xs] ++ combinations n xs

-- Problem 27
-- Group the elements of a set into disjoint subsets. 
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs = [([],xs)]
combination n [] = []
combination n (x:xs) = ts ++ ds
  where ts = [(x:ys,zs) | (ys,zs) <- combination (n-1) xs]
        ds = [(ys,x:zs) | (ys,zs) <- combination  n    xs]
              
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]    
group (n:ns) xs = [g:gs | (g,rs) <- combination n xs
                        , gs <- group ns rs]

group' :: [Int] -> [a] -> [[[a]]]
group' [] = const [[]]
group' (n:ns) = concatMap (uncurry $ (. group' ns) . map . (:)) . combination n

group'' :: [Int] -> [a] -> [[[a]]]
group'' [] xs = [[]]
group'' (g:gs) xs = concatMap helper $ combination g xs
  where helper (as, bs) = map (as:) (group'' gs bs)

-- Problem 28
-- Sorting a list of lists according to length of sublists  
sortBy' f = qsort f
  where qsort f [] = []
        qsort f (x:xs) = lesser ++ [x] ++ higher
          where lesser = qsort f [e | e <- xs, f e x /= GT]
                higher = qsort f [e | e <- xs, f e x == GT]                

lsort = sortBy' (\a b -> (length a) `compare` (length b))

splitWhen p xs = go p xs []
  where go p [] acc = (acc,[])
        go p ys@(x:xs) acc
          | p x = (acc,ys)
          | otherwise = go p xs (acc++[x]) 

update p f xs = let (fs,rs) = splitWhen p xs
                in case rs of
                  [] -> fs
                  (r:rs') -> fs ++ (f r:rs')
                    
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([],[])
span' p xs@(x:xs')
  | p x = let (ps,ns) = span' p xs' in (x:ps,ns)
  | otherwise = ([],xs)

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' p (x:xs) = (x:ys) : groupBy' p zs 
  where (ys,zs) = span' (p x) xs

-- This function sorts based on frequency of their length occurance
lfsort :: [[a]] -> [[a]]
lfsort lists = concat groups 
  where groups = lsort $ groupBy' equalLength $ lsort lists
        equalLength x y = length x == length y
