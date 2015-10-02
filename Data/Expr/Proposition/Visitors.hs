-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------

type Idents = Set Ident

freeVars :: Expr -> Idents
freeVars
  = visit $
    undefined
    
type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env
  = visit $
    undefined

eval :: Expr -> Bool
eval
  = visit $
    undefined

-- ----------------------------------------
