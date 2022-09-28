-- 1)

flatten :: [[Int]] -> [Int]
flatten l = foldl (++) [] l


-- 2)
myLength :: String -> Int
myLength s = foldl (+) 0 $ map (const 1) s


-- 3)
myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


-- 4)
countln :: [[Int]] -> Int -> [Int]
countln _ _ = []


-- 5)
firstWord :: String -> String
firstWord s = takeWhile (/= ' ') s
