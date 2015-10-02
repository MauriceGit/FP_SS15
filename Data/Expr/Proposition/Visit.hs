-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visit where

import Data.Expr.Proposition.Types

-- ----------------------------------------

data Visitor r
  = V { vLit    :: Bool  -> r
      , vVar    :: Ident -> r
      , vUnary  :: Op1   -> r -> r
      , vBinary :: Op2   -> r -> r -> r
      }

idExpr :: Visitor Expr
idExpr
  = undefined

visit :: Visitor r -> Expr -> r
visit v e = undefined

-- ----------------------------------------
