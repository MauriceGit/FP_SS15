-- | expressions with unary and binary operators
-- and conditionals, with integer and bool values
-- and free and bound variables

module Data.Expr.Imperative.Types where

import Data.Pretty

data Expr
  = BLit   Bool
  | ILit   Integer
  | Var    Ident
  | Unary  Op1   Expr
  | Binary Op2   Expr Expr
  | Cond   Expr  Expr Expr
  | While  Expr  Expr
  | Try    Expr  Expr
  | Read   String
  | Write  String Expr
  deriving (Eq, Ord, Show)

type Ident = String

-- ----------------------------------------

data Op1
  = Not                      -- unary boolean
  | ToInt                    -- conversion from Bool to Integer
  | UPlus | UMinus | Signum  -- unary integer
  | PreIncr  | PreDecr       -- ++i, --i
  | PostIncr | PostDecr      -- i++, i--
  deriving (Eq, Ord, Show)

data Op2
  = And  | Or    | Impl | Xor | Equiv    -- boolean
  | Plus | Minus | Mult | Div | Mod      -- integer
  | Eq   | Neq   | Ge   | Gr  | Le | Ls  -- relational
  | Seq                                  -- sequence operation
  | Assign                               -- assignments
  deriving (Eq, Ord, Show)

-- ----------------------------------------
--
-- simple pretty printer for expressions

instance Pretty Expr where
  pretty (BLit b)            = pretty b
  pretty (ILit i)            = pretty i
  pretty (Var    x)          = x
  pretty (Unary op e1)
    | isPost op              = "(" ++ pretty e1 ++ pretty op ++ ")"
    | otherwise              = "(" ++ pretty op ++ pretty e1 ++ ")"
  pretty (Binary op e1 e2) = "(" ++
                             pretty e1
                             ++ " " ++
                             pretty op
                             ++ " " ++
                             pretty e2
                             ++ ")"
  pretty (Cond   c  e1 e2) = "if " ++
                             pretty c
                             ++ " then " ++
                             pretty e1
                             ++ " else " ++
                             pretty e2
                             ++ " fi"
  pretty (While c b)       = "while " ++
                             pretty c
                             ++ " do " ++
                             pretty b
                             ++ " done"
  pretty (Try e1 e2)       = "try " ++
                             pretty e1
                             ++ " catch " ++
                             pretty e2
                             ++ " done"
  pretty (Read s)          = "read" ++
                             ( if null s
                               then ""
                               else " " ++ show s
                             )
  pretty (Write s e)       = "(write" ++
                             ( if null s
                               then ""
                               else " " ++ show s
                             ) ++ " " ++
                             pretty e
                             ++ ")"
  
-- ----------------------------------------

isPost :: Op1 -> Bool
isPost = (`elem` [PostIncr, PostDecr])

instance Pretty Op1 where
  pretty Not        = "not"
  pretty ToInt      = "ord"
  pretty UPlus      = "+"
  pretty UMinus     = "-"
  pretty Signum     = "signum"
  pretty PreIncr    = "++"
  pretty PreDecr    = "--"
  pretty PostIncr   = "++"
  pretty PostDecr   = "--"

  
instance Pretty Op2 where
  pretty And        = "&&"
  pretty Or         = "||"
  pretty Impl       = "=>"
  pretty Xor        = "<+>"
  pretty Equiv      = "<=>"

  pretty Plus       = "+"
  pretty Minus      = "-"
  pretty Mult       = "*"
  pretty Div        = "/"
  pretty Mod        = "%"

  pretty Eq         = "=="
  pretty Neq        = "/="
  pretty Gr         = ">"
  pretty Ge         = ">"
  pretty Ls         = "<"
  pretty Le         = "<="

  pretty Assign     = ":="
  pretty Seq        = ","
  
-- ----------------------------------------

