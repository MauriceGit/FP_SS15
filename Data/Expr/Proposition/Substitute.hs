module Data.Expr.Proposition.Substitute where

import Data.List      (union, nub)


import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env e = substVars' e 
    where
    
        substVars' (Lit e) = Lit e
        -- Mit selbstgebautem: maybe
        -- substVars' (Var i) = lookup' (lookup i env) (Var i)
        substVars' (Var i) = maybe (Var i) id (lookup i env)
        substVars' (Unary f e) = Unary f (substVars' e)
        substVars' (Binary f e1 e2) = Binary f (substVars' e1) (substVars' e2)
        
        -- Die Funktion ist optional, tut das Gleiche, wie maybe
        -- maybe wendet das zweite Argument auf das dritte Argument an,
        -- sofern dieses Just a ist. Und gibt sonst das erste zurück.
        -- Also das Gleich, wie lookup'.
        lookup' :: Maybe Expr -> Expr -> Expr
        lookup' (Just i) _ = i
        lookup' Nothing i = i

-- Testaufruf:
-- substVars [("a", Lit False)] ( either (error.show) id (parse' "a && b"))
        

freeVars :: Expr -> [Ident]
freeVars e = freeVars' e
    where
    freeVars' (Lit _) = []
    freeVars' (Var i) = [i]
    freeVars' (Unary _ e) = freeVars' e
    freeVars' (Binary _ e1 e2) = freeVars' e1 `union` freeVars' e2

-- Testaufruf:
-- freeVars (either (error.show) id  (parse' "a && true || b => c"))
