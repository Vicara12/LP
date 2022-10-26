data Tree a = Empty | Node a (Tree a) (Tree a)
         deriving (Show)

-- 1)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a lt rt) = a:(preorder lt)++(preorder rt)

instance Foldable Tree where
    foldr f n t = foldr f n (preorder t)


-- 2)

avg :: Tree Int -> Double
avg t = fromIntegral(sum t)/fromIntegral(length t)

-- 3)

cat :: Tree String -> String
cat Empty = ""
cat t = init $ foldr join "" t
    where
        join a b = a++" "++b
