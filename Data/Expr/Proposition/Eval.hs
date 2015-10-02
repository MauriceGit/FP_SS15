-- | evaluation of boolean expressions without free variables

module Data.Expr.Proposition.Eval where

import Data.Expr.Proposition.Types

-- ----------------------------------------

eval :: Expr -> Bool
eval (Lit    b)        = b
eval (Var    x)        = error $ unwords ["free variable", show x, "in expression"]
eval (Unary  op e)     = mf1 op (eval e)
eval (Binary op e1 e2) = mf2 op (eval e1) (eval e2)

mf1 :: Op1 -> (Bool -> Bool)
mf1 Not = not

mf2 :: Op2 -> (Bool -> Bool -> Bool)
mf2 And   = (&&)
mf2 Or    = (||)
mf2 Impl  = (<=)
mf2 Xor   = (/=)
mf2 Equiv = (==)

-- ----------------------------------------
