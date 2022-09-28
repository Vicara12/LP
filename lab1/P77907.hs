-- 1)

absValue :: Int -> Int
absValue n
    | n >= 0    = n
    | otherwise = -n


-- 2)

power :: Int -> Int -> Int
power _ 0 = 1
power x p = x * power x (p-1)


-- 3)

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = isPrime' n (n-1)
    where
        isPrime' :: Int -> Int -> Bool
        isPrime' n d
            | d < 2     = True
            | otherwise = not (mod n d == 0) && (isPrime' n (d-1))


-- 4)

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = (slowFib (n-1)) + (slowFib (n-2))


-- 5)

quickFib :: Int -> Int
quickFib n = fst $ quickFib' n
    where
        quickFib' :: Int -> (Int, Int)
        quickFib' 0 = (0, 0)
        quickFib' 1 = (1, 0)
        quickFib' n = (fst res + snd res, fst res)
            where
                res = quickFib' (n-1)
