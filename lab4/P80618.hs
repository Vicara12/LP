-- 1)

data Queue a = Queue [a] [a]
     deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = Queue l1 (x:l2)

pop :: Queue a -> Queue a
pop (Queue [] l2) = pop (Queue (reverse l2) [])
pop (Queue (x:xs) l2) = (Queue xs l2)

top :: Queue a -> a
top (Queue [] l2) = top (Queue (reverse l2) [])
top (Queue (x:xs) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False


-- 2)

instance Eq a => Eq (Queue a)
     where
        (Queue q1 q2) == (Queue p1 p2) = ((length q1) + (length q2) == (length p1) + (length p2)) && (and $ zipWith (==) (q1++(reverse q2)) (p1++(reverse p2)))
