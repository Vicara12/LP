-- 1)

ones :: [Integer]
ones = 1 : ones


-- 2)

nats :: [Integer]
nats = 0 : map (+1) nats


-- 3)

ints :: [Integer]
ints = tail $ concat $ map duplicate nats
    where
        duplicate x = [x,-x]


-- 4)

triangulars :: [Integer]
triangulars = tail $ scanl (+) 0 $ tail nats


-- 5)

factorials :: [Integer]
factorials = scanl (*) 1 $ tail nats


-- 6)

fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs $ tail fibs)


-- 7)

primes :: [Integer]
primes = p [2 ..]
    where
        p (f:numeros) = f:[n | n <- numeros, mod n f /= 0]


-- 8)

merge :: [Integer] -> [Integer] -> [Integer] -> [Integer]
merge l1 l2 l3 = [1 ..]

hammings :: [Integer]
hammings = 1 : merge (map (*2) hammings) (map (*3) hammings) (map (*5) hammings)
