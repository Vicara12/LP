-- 1)

eql :: [Int] -> [Int] -> Bool
eql l1 l2 = length l1 == length l2 && (and $ zipWith (==) l1 l2)


-- 2)

prod :: [Int] -> Int
prod l = foldl (*) 1 l


-- 3)

prodOfEvens :: [Int] -> Int
prodOfEvens l = foldl (*) 1 $ filter even l


-- 4)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1


-- 5)
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = foldl (+) 0 $ zipWith (*) l1 l2
