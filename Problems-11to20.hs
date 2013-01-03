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

