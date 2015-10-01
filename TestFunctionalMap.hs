module TestFunctionalMap
where

import           Data.FunctionalMap
import           Prelude            hiding (lookup)
import qualified Prelude            as P
import           Test.QuickCheck

prop_1 :: String -> Bool
prop_1 xs = undefined

prop_2 :: String -> Bool
prop_2 xs = undefined

prop_3 :: String -> Bool
prop_3 xs = undefined

-- ----------------------------------------

quickCheck' :: Testable prop => prop -> IO ()
quickCheck' = quickCheckWith stdArgs{maxSuccess=200}

main
  = do quickCheck'  prop_1
       quickCheck'  prop_2
       quickCheck'  prop_3
       
-- ----------------------------------------

