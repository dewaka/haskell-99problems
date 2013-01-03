-- Question 1 to 10 - Lists

-- 1) Find the last element of a list
myLast :: [a] -> a
myLast = head . reverse

-- 2) Find last but one element of a list
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- 3) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: (Eq a, Num a) => [t] -> a -> t
elementAt (x:_) 1 = x 
elementAt (_:xs) n = elementAt xs (n-1)
elementAt [] _ = error "Index too large"
                 
-- 4) Find the number of elements of a list. 
myLength :: Num a => [t] -> a
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5) Reverse a list.
myReverse :: [a] -> [a]
myReverse = loop []
    where loop acc [] = acc
          loop acc (x:xs) = loop (x:acc) xs

-- 6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (xs == myReverse xs)

-- 7) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- 8) Eliminate consecutive duplicates of list elements.

compress :: Eq a => [a] -> [a]
compress = loop []
    where loop acc [] = reverse acc
          loop [] (x:xs) = loop [x] xs
          loop acc@(y:_) (x:xs)
               | x == y = loop acc xs
               | otherwise = loop (x:acc) xs
                   
-- 9) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

-- pack "aaaabccaadeeee" -> ["aaaa", "b", "cc", "aa", "d", "eeee"]

pack :: Eq a => [a] -> [[a]]
pack = loop []
       where loop acc [] = reverse acc
             loop [] (x:xs) = loop [[x]] xs
             loop acc@(y:ys) (x:xs) = if (head y == x)
                                      then loop ([(x : y)] ++ ys) xs
                                      else loop ([x] : acc) xs

-- 10) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: Eq b => [b] -> [(Int, b)]
encode x = zip (map length y) (map head y)
           where y = pack x