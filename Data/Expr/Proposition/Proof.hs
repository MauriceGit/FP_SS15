module Data.Expr.Proposition.Proof where

import           Data.Expr.Proposition.Constr
import           Data.Expr.Proposition.Eval
import           Data.Expr.Proposition.Substitute
import           Data.Expr.Proposition.Types
import           Data.Pretty

import           Data.List                        (intercalate)
import           Data.Maybe                       (listToMaybe)

-- ----------------------------------------

truthTable :: Int -> [[Bool]]
truthTable 0 = [] 
truthTable 1 = [[True],[False]]
truthTable n = append' True ++ (append' False)
    where
    append' b = map ([b]++) tRec
    tRec = truthTable (n-1)

-- compute a proof by generating a truth table,
-- iterate over all rows in the table
-- and substitute all variable by the values in a row
-- and evaluate the expression
-- if a single result is false
-- we have a counter example, else the expr
-- is a tautology

proof' :: Expr -> Maybe VarEnv
proof' e = proof'' table e
    where
        -- proof'' bekommt eine Reihe der Wahrheitstafel übergeben!
        proof'' [] _ = Nothing -- > Tautologie!
        proof'' (x:xs) e 
            -- Eine Variablenbelegung wertet zu False aus!
            | not (eval (substVars env e)) = Just env
            | otherwise = proof'' xs e
            where
                -- eine bestimmte Möglichkeit für eine Variablenbelegung!
                -- das Map ist nur dafür da, um aus Bool --> Lit Bool, also eine gleichwertige Expression zu machen
                env = zip vars (map (Lit . id) x)
        
        -- freie Variablen sind immer konstant gleich!
        vars = freeVars e
        -- table ist immer konstant gleich!
        table = truthTable (length vars)
    
-- Testfall:
-- proof ( either (error.show) id (parse' "true && true || false => b"))
-- proof ( either (error.show) id (parse' "b && c || a => b"))

proof :: Expr -> String
proof e
  = case proof' e of
     Nothing
       -> pretty e ++ " is a tautology"
     Just env
       -> pretty e ++ " isn't a tautology, " ++
          "a counter example is " ++ ppEnv env
  where
    ppEnv = intercalate ", " . map ppVar
    ppVar (i, v) = i ++ "=" ++ pretty v

-- ----------------------------------------
