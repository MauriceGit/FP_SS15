module TestFunctionalList
where

import           Data.FunctionalList
import           Prelude hiding (foldl, foldr, map)
import qualified Prelude as P
import           Test.QuickCheck

prop_fromTo :: String -> Bool
prop_fromTo xs
  = toList (fromList xs) == xs

prop_cons :: String -> Bool
prop_cons xs
  = toList (P.foldr cons empty xs) == xs 

prop_snoc :: String -> Bool
prop_snoc xs
  = toList (P.foldl snoc empty xs) == xs 

prop_append :: String -> Bool
prop_append xs0
  = toList (build xs0) == xs0
  where
    build [] = empty
    build [x] = singleton x
    build xs = build xs1 `append` build xs2
      where
        (xs1, xs2) = P.splitAt (length xs `div` 2) xs
        

testFL :: IO ()
testFL
  = do quickCheck prop_fromTo
       quickCheck prop_cons
       quickCheck prop_snoc
       quickCheck prop_append

-- ----------------------------------------
--
-- performance test
--
-- ghci: :set +s
--
-- direct test with left and right associated lists of lists
--
-- test1, test3, test4 are O(n)
-- test2 is O(n^2)

test1, test2, test3, test4 :: Int -> [Int]

test1 n = P.foldr (++) [] $ P.map (:[]) [1..n]
test2 n = P.foldl (++) [] $ P.map (:[]) [1..n]

test3 n = ($ []) $ P.foldr append empty $ P.map singleton [1..n]
test4 n = ($ []) $ P.foldl append empty $ P.map singleton [1..n] -- !!!

-- ----------------------------------------

main :: IO ()
main = testFL

-- ----------------------------------------
