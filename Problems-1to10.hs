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

-- Work in progress

-- flatten (Elem x) = [x]
-- flatten (List (x:xs)) = (flatten x) ++ (flatten xs)

-- 8) Eliminate consecutive duplicates of list elements.

-- Work in progress

compress = loop []
    where loop acc [] = acc
          loop acc@(y:_) (x:xs)
               | x == y = loop acc xs
               | otherwise = loop (x:acc) xs
                   
