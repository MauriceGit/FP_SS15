{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Expr.ArithmLogic.EvalErrorMonad where

import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.Except

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

isB :: Value -> Bool
isB (B _) = True
isB _     = False

isI :: Value -> Bool
isI (I _) = True
isI _     = False

-- ----------------------------------------

data Result a
  = R { resVal :: a         }
  | E { resErr :: EvalError }
    deriving (Show)

instance Functor Result where
  fmap = undefined

instance Applicative Result where
  pure = return
  (<*>) = ap
  
  
instance Monad Result where
  return x = R x
  (R x) >>= f = f x
  (E x) >>= f = E x


-- class (Monad m) => MonadError e m | m -> e where
--    throwError :: e -> m a
--    catchError :: m a -> (e -> m a) -> m a

instance MonadError EvalError Result where
  throwError = E
  catchError (E x) f = f x
  catchError (R x) f  = R x
  
instance (Pretty a) => Pretty (Result a) where
  pretty (R x) = pretty x
  pretty (E e) = "error: " ++ pretty e

-- ----------------------------------------
-- error handling
  
data EvalError
  = FreeVar String
  | NotImpl String
  | ValErr  String Value 
  | Div0
  | Mzero
  deriving (Show)

instance Pretty EvalError where
  pretty (FreeVar i)  = "free variable " ++ show i ++ " in expression"
  pretty (NotImpl n)  = n ++ " not implemented"
  pretty (ValErr e g) = e ++ " value expected, but got: " ++ pretty g
  pretty Div0         = "divide by zero"
  pretty Mzero        = "mzero"

boolExpected :: Value -> Result a
boolExpected = throwError . ValErr "Bool"

intExpected :: Value -> Result a
intExpected  = throwError . ValErr "Integer"

notImpl :: String -> Result a
notImpl = throwError . NotImpl

freeVar :: String -> Result a
freeVar = throwError . FreeVar

div0 :: Result a
div0  = throwError Div0

-- ----------------------------------------

eval :: Expr -> Result Value
eval (BLit b)          = return (B b)
eval (ILit i)          = return (I i)
eval (Var    x)        = freeVar x -- E (FreeVar ("free variable " ++ show x ++ " in expression"))
eval (Unary  op e1)    = do v1  <- eval e1
                            mf1 op v1
                            
eval (Binary op e1 e2) = do v1  <- eval e1
                            v2  <- eval e2
                            mf2 op v1 v2
                            
eval (Cond   c e1 e2)  = do b <- evalBool c
                            if b
                              then eval e1
                              else eval e2
                              
eval (Let _x _e1 _e2)  = notImpl "let expressions"


evalBool :: Expr -> Result Bool
evalBool e
  = do r <- eval e
       case r of
        (B b) -> return b
        i -> boolExpected i
        
{--        
evalBool :: Expr -> Result Bool
evalBool e
  = do r <- eval e
       case r of
        (B b) -> return b
        (I _) -> error "bool result expected"
--}

-- ----------------------------------------
-- MF: Meaning function

-- mf1 :: Op1 -> MF1
-- mf1 Not        = op1BB not
-- mf1 ToInt      = op1BI (toInteger . fromEnum)
-- mf1 UPlus      = op1II id
-- mf1 UMinus     = op1II (0 -)
-- mf1 Signum     = op1II signum
-- mf1 UPlusMinus = \ _ -> error "+/- not yet implemented"

type MF1 = Value -> Result Value

mf1 :: Op1 -> MF1
mf1 Not        = op1BB not
mf1 ToInt      = op1BI (toInteger . fromEnum)
mf1 UPlus      = op1II id
mf1 UMinus     = op1II (0 -)
mf1 Signum     = op1II signum
mf1 op         = (\ value -> notImpl(show op))-- ODER AUCH: const $ notImpl (show op)                 
-- Der zweite Parameter (also das b / value) wird ignoriert.
  
op1BB :: (Bool -> Bool) -> MF1
op1BB op (B b) = return $ B (op b)
op1BB _  v     = boolExpected v -- E (ValErr "Ist kein Boolean!" v) -- Woher wissen wir, dass v hier ein Integer ist?

op1II :: (Integer -> Integer) -> MF1
op1II op (I i) = return (I (op i))
op1II _  v     = intExpected v -- E (ValErr "Ist kein Integer!" v)

op1BI :: (Bool -> Integer) -> MF1
op1BI op (B b) = return (I (op b))
op1BI _  v     = boolExpected v --E (ValErr "Ist kein Boolean!" v)

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
mf2 Div       = divIII div
mf2 Mod       = divIII mod
mf2 Eq        = op2IIB (==)
mf2 Neq       = op2IIB (/=)
mf2 Ge        = op2IIB (>=)
mf2 Gr        = op2IIB (>)
mf2 Le        = op2IIB (<=)
mf2 Ls        = op2IIB (<)
mf2 op        = \ _ _ -> notImpl (pretty op)

op2BBB :: (Bool -> Bool -> Bool) -> MF2
op2BBB op (B b1) (B b2) = return (B (b1 `op` b2))
op2BBB _  v1     (B b)     = boolExpected v1
op2BBB _  (B b)     v2     = boolExpected v2

op2III :: (Integer -> Integer -> Integer) -> MF2
op2III op (I i1) (I i2) = return (I (i1 `op` i2))
op2III _  v1      (I i2)    = intExpected v1
op2III _  (I i1)      v2    = intExpected v2

op2IIB :: (Integer -> Integer -> Bool) -> MF2
op2IIB op (I i1) (I i2) = return (B (i1 `op` i2))
op2IIB _  v1      (I i2)    = intExpected v1
op2IIB _  (I i1)      v2    = intExpected v2


divIII :: (Integer -> Integer -> Integer) -> MF2
divIII op (I x) (I y)   = return (I (op x y))
divIII _  v1      (I i2)    = intExpected v1
divIII _  (I i1)      v2    = intExpected v2

-- ----------------------------------------
