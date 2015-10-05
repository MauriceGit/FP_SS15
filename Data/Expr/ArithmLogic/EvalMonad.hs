module Data.Expr.ArithmLogic.EvalMonad where

import Control.Applicative (Applicative(..))
import Control.Monad

import Data.Expr.ArithmLogic.Types
import Data.Pretty

-- ----------------------------------------
-- simple expression evaluation in monadic form
--
-- evaluates expressions without any free or bound variable

-- ----------------------------------------

data Value
  = B Bool
  | I Integer
    deriving (Eq, Ord, Show)
             
instance Pretty Value where
  pretty (B b) = pretty b
  pretty (I i) = pretty i

-- ----------------------------------------

newtype Result a
  = R {resVal :: a}
    deriving (Show)

instance Functor Result where
  fmap f (R x) = R (f x)

instance Applicative Result where
  pure = return
  (<*>) = ap
  
instance Monad Result where
  return = R
  (R x) >>= f = f x

instance (Pretty a) => Pretty (Result a) where
  pretty = pretty . resVal

-- ----------------------------------------

-- eval :: Expr -> Result Value
--
-- now eval works for every Monad
-- wen can generalize the signature
--
-- This also holds for evalBool, mf1 and mf2
  
eval :: Expr -> Result Value
eval (BLit b)          = return (B b)
eval (ILit i)          = return (I i)
eval (Var    x)        = error $ unwords ["free variable", show x, "in expression"]

eval (Unary  op e1)    = do v1  <- eval e1
                            mf1 op v1

eval (Binary op e1 e2) = do v1  <- eval e1
                            v2  <- eval e2
                            mf2 op v1 v2
                            
eval (Cond   c e1 e2)  = do b <- evalBool c
                            if b
                              then eval e1
                              else eval e2

eval (Let _x _e1 _e2)  = error "let expressions not implemented"

evalBool :: Expr -> Result Bool
evalBool e
  = do r <- eval e
       case r of
        (B b) -> return b
        (I _) -> error "bool result expected"
  
-- ----------------------------------------
-- MF: Meaning function

type MF1 = Value -> Result Value

mf1 :: Op1 -> MF1
mf1 Not        = op1BB not
mf1 ToInt      = op1BI (toInteger . fromEnum)
mf1 UPlus      = op1II id
mf1 UMinus     = op1II (0 -)
mf1 Signum     = op1II signum
mf1 UPlusMinus = \ _ -> error "+/- not yet implemented"

op1BB :: (Bool -> Bool) -> MF1
op1BB op (B b) = return $ B (op b)
op1BB _  _     = error "illegal operand with Bool -> Bool op"

op1II :: (Integer -> Integer) -> MF1
op1II op (I i) = return (I (op i))
op1II _  _     = error "illegal operand with Integer -> Integer op"

op1BI :: (Bool -> Integer) -> MF1
op1BI op (B b) = return (I (op b))
op1BI _  _     = error "illegal operand with Bool -> Integer op"

-- ----------------------------------------

type MF2 = Value -> Value -> Result Value

mf2 :: Op2 -> MF2
mf2 And       = op2BBB (&&)
mf2 Or        = op2BBB (||)
mf2 Impl      = op2BBB (<=)
mf2 Xor       = op2BBB (/=)
mf2 Equiv     = op2BBB (==)
mf2 Plus      = op2III (+)
mf2 Minus     = op2III (-)
mf2 Mult      = op2III (*)
mf2 Div       = op2III div
mf2 Mod       = op2III mod
mf2 Eq        = op2IIB (==)
mf2 Neq       = op2IIB (/=)
mf2 Ge        = op2IIB (>=)
mf2 Gr        = op2IIB (>)
mf2 Le        = op2IIB (<=)
mf2 Ls        = op2IIB (<)
mf2 op         = \ _ _ -> error (pretty op ++ " not yet implemented")

op2BBB :: (Bool -> Bool -> Bool) -> MF2
op2BBB op (B b1) (B b2) = return (B (b1 `op` b2))
op2BBB _  _      _      = error "illegal operand with op :: Bool -> Bool -> Bool"

op2III :: (Integer -> Integer -> Integer) -> MF2
op2III op (I i1) (I i2) = return (I (i1 `op` i2))
op2III _  _      _      = error "illegal operand with op :: Integer -> Integer -> Integer"

op2IIB :: (Integer -> Integer -> Bool) -> MF2
op2IIB op (I i1) (I i2) = return (B (i1 `op` i2))
op2IIB _  _      _      = error "illegal operand with op :: Integer -> Integer -> Bool"

-- ----------------------------------------
