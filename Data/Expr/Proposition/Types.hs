-- | data types for boolean expressions (propositions)

module Data.Expr.Proposition.Types
where

import Data.Pretty

data Expr
  = Lit    Bool
  | Var    Ident
  | Unary  Op1 Expr
  | Binary Op2 Expr Expr
  deriving (Eq, Ord, Show)

type Ident = String

data Op1
  = Not
  deriving (Eq, Ord, Show)

data Op2
  = And | Or | Impl | Xor | Equiv
  deriving (Eq, Ord, Show)

-- ----------------------------------------
-- simple pretty printer for Expr, Op1, Op2 and Bool

instance Pretty Expr where
  pretty (Lit    b)         = pretty b
  pretty (Var    x)        = x
  pretty (Unary  op e1)    = pretty op
                             ++ " " ++
                             pretty e1
  pretty (Binary op e1 e2) = "(" ++
                             pretty e1
                             ++ " " ++
                             pretty op
                             ++ " " ++
                             pretty e2
                             ++ ")"

instance Pretty Op1 where
  pretty Not = "not"

instance Pretty Op2 where
  pretty And   = "&&"
  pretty Or    = "||"
  pretty Impl  = "=>"
  pretty Xor   = "<+>"
  pretty Equiv = "<=>"

-- ----------------------------------------
