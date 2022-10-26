-- 1)

myMap :: (a -> b) -> [a] -> [b]
myMap f l = [f x | x <- l]


-- 2)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter c l = [ x | x <- l, c x]


-- 3)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = [f a b | (a,b) <- zip l1 l2]


-- 4)

thingify :: [Int] -> [Int] -> [(Int,Int)]
thingify l1 l2 = [(a,b) | a <- l1, b <- l2, mod a b == 0]


-- 5)

factors :: Int -> [Int]
factors n = [f | f <- [1 .. n], mod n f == 0]
