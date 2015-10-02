module Data.Expr.Proposition.Substitute where

import Data.List      (union, nub)

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env e = undefined

freeVars :: Expr -> [Ident]
freeVars e = freeVars' e
	where
	freeVars' (Lit _) = []
	freeVars' (Var i) = [i]
	freeVars' (Unary _ e) = freeVars' e
	freeVars' (Binary _ e1 e2) = freeVars' e1 `union` freeVars' e2

-- freeVars (either (error.show) id  (parse' "a && true || b => c"))
