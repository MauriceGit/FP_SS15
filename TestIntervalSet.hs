-- http://hackage.haskell.org/package/QuickCheck

module TestIntervalSet where

import           Data.IntervalSet
import           Test.QuickCheck

-- ----------------------------------------

-- invariant test: list of elems as input

prop_inv0 :: [Int] -> Bool
prop_inv0 = inv . fromList


-- invariant test: list of pairs

prop_inv :: [Pair] -> Bool
prop_inv = inv . fromPairs


-- all elements in set?
prop_elem :: [Pair] -> Bool
prop_elem ps
	= and (map isIn ps)
	where 
	s = fromPairs ps
	isIn (P (x,y)) = all (`member` s) [x..y]


-- ----------------------------------------
--
-- auxiliary data type for Arbitrary instance
-- of Pairs

newtype Pair = P (Int, Int)
          deriving (Show)

instance Arbitrary Pair where
  arbitrary
    = do x <- arbitrary
         y <- arbitrary
         return (P (x `min` y, x `max` y))

fromPairs :: [Pair] -> IntervalSet
fromPairs = fromIntervalList . map (\ (P p) -> p)

-- ----------------------------------------

testArgs :: Args
testArgs
  = stdArgs { maxSize=500
            , maxSuccess=200
            }

t0, t1, t2 :: IO ()
t0 = quickCheck prop_inv0
t1 = quickCheck (verbose prop_inv)
t2 = quickCheckWith testArgs prop_elem

main :: IO ()
main
  = do 
	t0
	t1
	t2
	

-- ----------------------------------------
