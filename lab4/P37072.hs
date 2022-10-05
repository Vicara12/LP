data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving (Show)

t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2


-- 1)

size :: Tree a -> Int
size Empty = 0
size (Node d al ar) = 1 + (size al) + (size ar)


-- 2)

height :: Tree a -> Int
height Empty = 0
height (Node a al ar) = 1 + max (height al) (height ar)


-- 3)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node d1 al1 ar1) (Node d2 al2 ar2) = (d1 == d2) && (equal al1 al2) && (equal ar1 ar2)
equal _ _ = False


-- 4)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic (Node d1 l1 r1) (Node d2 l2 r2) = (d1 == d2) && (((isomorphic l1 l2) && (isomorphic r1 r2)) || ((isomorphic l1 r2) && (isomorphic r1 l2)))
isomorphic _ _ = False


-- 5)

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node d l r) = d:((preOrder l)++(preOrder r))


-- 6)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node d l r) = (postOrder l)++(postOrder r)++[d]


-- 7)
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node d l r) = (inOrder l)++[d]++(inOrder r)


-- 8)

breadthFirst :: Tree a -> [a]
breadthFirst _ = []
