module Data.Expr.ArithmLogic.Eval where

import Data.Expr.ArithmLogic.Types
import Data.Pretty

-- ----------------------------------------

data Value
  = B Bool
  | I Integer
    deriving (Eq, Ord, Show)
             
instance Pretty Value where
  pretty (B b) = pretty b
  pretty (I i) = pretty i

-- ----------------------------------------
-- simple expression evaluation
--
-- evaluates expressions without any free or bound variable

eval :: Expr -> Value
eval (BLit b)          = B b
eval (ILit i)          = I i
eval (Var    x)        = error $ unwords ["free variable", show x, "in expression"]
eval (Unary  op e1)    = mf1 op (eval e1)
eval (Binary op e1 e2) = mf2 op (eval e1) (eval e2)
eval (Cond   c e1 e2)  =
  case eval c of
   (B b) 
     | b                 -> eval e1
     | otherwise         -> eval e2
   (I _)                 -> error "not a bool value in condition"
eval (Let _x _e1 _e2)  = error "let expressions not implemented"
      
-- ----------------------------------------

mf1 :: (Op1 -> Value -> Value)
mf1 Not        = op1BB not
mf1 ToInt      = op1BI (toInteger . fromEnum)
mf1 UPlus      = op1II id
mf1 UMinus     = op1II (0 -)
mf1 Signum     = op1II signum
mf1 UPlusMinus = \ _ -> error "+/- not yet implemented"

op1BB :: (Bool -> Bool) -> (Value -> Value)
op1BB op (B b) = B (op b)
op1BB _  _     = error "illegal operand with Bool -> Bool op"

op1II :: (Integer -> Integer) -> (Value -> Value)
op1II op (I i) = I (op i)
op1II _  _     = error "illegal operand with Integer -> Integer op"

op1BI :: (Bool -> Integer) -> (Value -> Value)
op1BI op (B b) = I (op b)
op1BI _  _     = error "illegal operand with Bool -> Integer op"

-- ----------------------------------------

mf2 :: (Op2 -> Value -> Value -> Value)
mf2 And        = op2BBB (&&)
mf2 Or         = op2BBB (||)
mf2 Impl       = op2BBB (<=)
mf2 Xor        = op2BBB (/=)
mf2 Equiv      = op2BBB (==)
mf2 Plus       = op2III (+)
mf2 Minus      = op2III (-)
mf2 Mult       = op2III (*)
mf2 Div        = op2III div
mf2 Mod        = op2III mod
mf2 Eq         = op2IIB (==)
mf2 Neq        = op2IIB (/=)
mf2 Ge         = op2IIB (>=)
mf2 Gr         = op2IIB (>)
mf2 Le         = op2IIB (<=)
mf2 Ls         = op2IIB (<)
mf2 op         = \ _ _ -> error (pretty op ++ " not implemented")

op2BBB :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
op2BBB op (B b1) (B b2) = B (b1 `op` b2)
op2BBB _  _      _      = error "illegal operand with op :: Bool -> Bool -> Bool"

op2III :: (Integer -> Integer -> Integer) -> (Value -> Value -> Value)
op2III op (I i1) (I i2) = I (i1 `op` i2)
op2III _  _      _      = error "illegal operand with op :: Integer -> Integer -> Integer"

op2IIB :: (Integer -> Integer -> Bool) -> (Value -> Value -> Value)
op2IIB op (I i1) (I i2) = B (i1 `op` i2)
op2IIB _  _      _      = error "illegal operand with op :: Integer -> Integer -> Bool"

-- ----------------------------------------
