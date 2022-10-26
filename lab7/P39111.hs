import Data.List (sort)
type Pos = (Int, Int)       -- lower left is (1,1)


-- 1)

dins :: Pos -> Bool
dins (c,r) = c >= 1 && c <=8 && r >= 1 && r <= 8


-- 2)

moviments :: Pos -> [Pos]
moviments (r,c) = filter dins rawMoviments
    where
        rawMoviments = [(r+2,c+1),(r+2,c-1),
                        (r-2,c+1),(r-2,c-1),
                        (r+1,c+2),(r-1,c+2),
                        (r+1,c-2),(r-1,c-2)]


-- 3)

potAnar3 :: Pos -> Pos -> Bool
potAnar3 orig dest = elem dest all_pos3
    where
        all_pos1 = moviments orig
        all_pos2 = concatMap moviments all_pos1
        all_pos3 = concatMap moviments all_pos2


-- 4)

potAnar3' :: Pos -> Pos -> Bool
potAnar3' orig dest = elem dest all_pos3
    where
        all_pos3 = [orig] >>= moviments >>= moviments >>= moviments
