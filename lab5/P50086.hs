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


-- 1)

instance Functor Queue where
    fmap f (Queue l1 l2) = Queue (fmap f l1) (fmap f l2)


-- 2)

translation :: Num b => b -> Queue b -> Queue b
translation b q = fmap ((+) b) q


-- 3)

instance Applicative Queue where
    pure a = push a create
    (<*>) f (Queue l1 l2) = Queue (f<*>l1) (f<*>l2)

instance Monad Queue where
    return a = pure a
    (>>=) f (Queue l1 l2) = 

-- 4)

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f q = do
    elm <- q
    if f elm then return elm else create


