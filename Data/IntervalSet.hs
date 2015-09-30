module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
    | emptyInterval (x1, y1) || emptyInterval (x2, y2) = False
    | x1 > x2 = overlap (x2, y2) (x1, y1)
    | otherwise = emptyInterval(y1+1, x2-1)


less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
  = y1 <= x2

                           
emptyInterval :: Interval -> Bool
emptyInterval (x, y)
  = x > y -- awesome


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv is = and (map (not . emptyInterval) is) -- Keins ist leer
        && and (zipWith blubb is (tail is))
        where blubb i1 i2 = not (overlap i1 i2) && (less i1 i2)

-- Alle Intervalle nicht leer, Keine Überlappung, Aufsteigend sortiert


-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval i [] = [i]
insertInterval i (ix : ixs) 
    | less i ix && not (overlap i ix) = i : ix : ixs
    | overlap i ix = insertInterval (merge i ix) ixs
    | otherwise = ix : insertInterval i ixs


fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList [] = []
fromIntervalList (x:xs) 
    | emptyInterval x = fromIntervalList xs
    | otherwise = insertInterval x (fromIntervalList xs) -- wenn tupel !leer, dann insert


-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

union :: IntervalSet -> IntervalSet -> IntervalSet
union [] i2 = i2
union i1 [] = i1
union (x:xs) i2 = insertInterval x (union xs i2)

member :: Int -> IntervalSet -> Bool
member i is = or (map (\ (x,y) -> x <= i && y >= i) is)

         
fromList :: [Int] -> IntervalSet 
fromList [] = []
fromList (i:is) = insert i (fromList is)


toList :: IntervalSet -> [Int]
toList [] = []
toList is =  foldr (\ (x1,x2) xs -> [x1..x2] ++ xs) [] is


-- ----------------------------------------
