module TruthTable
where

import           Data.List (intercalate)


type TruthTable = [[Bool]]

-- | generate a truth table for n variables

truthTable :: Int -> TruthTable
truthTable n = undefined

ppTruthTable :: TruthTable -> String
ppTruthTable tt
  = unlines (map ppRow tt)
  where
    ppRow r = intercalate " " (map (take 1 . show) r)

printTT :: TruthTable -> IO ()
printTT = putStrLn . ppTruthTable

-- ----------------------------------------
