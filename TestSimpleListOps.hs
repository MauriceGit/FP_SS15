module TestSimpleListOps
where

import           Data.List.SimpleOps

import qualified Data.List as L
import           Prelude hiding (splitAt)
import           Test.QuickCheck

-- ----------------------------------------

prop_nub :: String -> Bool
prop_nub xs
  = nub xs == L.nub xs

prop_nub' :: String -> Bool
prop_nub' xs
  = nub' xs == L.nub xs

prop_nub'' :: String -> Bool
prop_nub'' xs
  = nub xs == L.nub xs

-- ----------------------------------------

prop_splitAt :: String -> Property
prop_splitAt xs
  = forAll (elements [0..length xs]) $ \ i ->
    splitAt' i xs == splitAt i xs

-- ----------------------------------------

prop_join'split' :: Char -> String -> Bool
prop_join'split' c xs
  = join' c (split' c xs) == xs

-- ----------------------------------------


-- ----------------------------------------

quickCheck' :: Testable prop => prop -> IO ()
quickCheck' = quickCheckWith stdArgs{maxSuccess=1000}

testNub :: IO ()
testNub
  = mapM_ quickCheck'
    [ prop_nub
    , prop_nub'
    , prop_nub''
    ]

testSplit :: IO ()
testSplit
  = do quickCheck  prop_join'split'

main :: IO ()
main
  = do testNub
       testSplit
       
-- ----------------------------------------


