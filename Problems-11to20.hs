-- 11) Modified run-length encoding.

data Encoding a n = Single a | Multiple n a deriving (Show)

                  
pack :: Eq a => [a] -> [[a]]
pack = loop []
       where loop acc [] = reverse acc
             loop [] (x:xs) = loop [[x]] xs
             loop acc@(y:ys) (x:xs) = if (head y == x)
                                      then loop ([(x : y)] ++ ys) xs
                                      else loop ([x] : acc) xs

encode :: Eq b => [b] -> [(Int, b)]
encode x = zip (map length y) (map head y)
           where y = pack x

-- This is a very very lazy kind of solution... but it does the job
encodeModified x = map (\(n, x) -> if n == 1 then (Single x) else (Multiple n x)) y
    where y = encode x

-- 12) Decode a run-length encoded list.

decodeModified :: [Encoding a Int] -> [a]
decodeModified = loop []
    where loop acc [] = reverse acc
          loop acc (x:xs) = case x of
                              (Single a) -> loop (a : acc) xs
                              (Multiple n a) -> loop ((take n (repeat a)) ++ acc) xs
              
skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile p [] = []              
skipWhile p ls@(x:xs) = if not (p x)
                     then ls
                     else skipWhile p xs

breakDown :: Eq a => [a] -> [[a]]
breakDown = loop []
    where loop acc [] = reverse acc
          loop acc ls@(x:xs) = loop ((takeWhile (== x) ls) : acc) (skipWhile (== x) ls)

-- 14) Duplicate the elements of a list.
dupli xs = foldr (++) [] [replicate 2 x | x <- xs]

-- 15) Replicate the elements of a list a given number of times.
repli xs n = foldr (++) [] [replicate n x | x <- xs]

-- 16)  Drop every N'th element from a list.
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

-- 17) Split a list into two parts; the length of the first part is given.
-- split "abcdefghik" 3 -> ("abc", "defghik")
split xs n = loop [] xs n
    where loop acc xs 0 = [acc, xs]
          loop acc (x:xs) n = loop (acc ++ [x]) xs (n-1)

-- 18) Extract a slice from a list.
slice xs m n = take (n - m + 1) . drop (m - 1) $ xs

-- 19) Rotate a list N places to the left.
rotate xs n
       | n >= 0 = drop n xs ++ take n xs
       | otherwise = rotate xs ((length xs) + n)

-- 20) Remove the K'th element from a list.
removeAt n xs = loop n xs []
    where loop 1 (x:xs) acc = (x, acc ++ xs)
          loop n (x:xs) acc = loop (n-1) xs (acc ++ [x])
