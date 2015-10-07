-- | expressions with unary and binary operators
-- and conditionals, with integer and bool values
-- and free and bound variables

module Data.Expr.Imperative.Types where

import Data.Pretty

data Expr
  = BLit   { _bval :: Bool    }
  | ILit   { _ival :: Integer }
  | Var    { _var  :: Ident   }
  | Unary  { _op1  :: Op1
           , _e1   :: Expr
           }
  | Binary { _op2  :: Op2
           , _e1   :: Expr
           , _e2   :: Expr
           }
  | Cond   { _cond :: Expr
           , _e1   :: Expr
           , _e2   :: Expr
           }
  | While  { _cond :: Expr
           , _body :: Expr
           }
  | Read   { _msg  :: String }
  | Write  { _msg  :: String
           , _e1   :: Expr
           }
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

