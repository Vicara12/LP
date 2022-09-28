-- 1)

myLength :: [Int] -> Int
myLength [] = 0
myLength l = 1 + myLength (tail l)

-- 2)

myMaximum :: [Int] -> Int
myMaximum l
    | length l == 1 = head l
    | otherwise       = if current_max < first then first else current_max
    where
        current_max = myMaximum $ tail l
        first = head l


-- 3)

average :: [Int] -> Float
average l = fromIntegral (sum l) / fromIntegral (length l)


-- 4)

buildPalindrome :: [Int] -> [Int]
buildPalindrome l = buildPalindrome'
