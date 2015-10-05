{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Expr.ArithmLogic.EvalReaderErrorTrans
       ( EvalError
       , Value
       , eval'
       )
where

import Control.Applicative   (Applicative(..))
import Control.Monad         ()
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader

import Data.Expr.ArithmLogic.Types
import Data.Pretty

import qualified Data.Map as M

-- ----------------------------------------
-- simple expression evaluation in monadic form
--
-- evaluates expressions with free variables and local let bindings
--
-- a monad stack with reader + error + identity is
-- used instead of a native monad

-- ----------------------------------------
--
-- the pure value

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

newtype Env = Env (M.Map Ident Value)

emptyEnv :: Env
emptyEnv = Env M.empty

lookupEnv :: Ident -> Env -> Maybe Value
lookupEnv i (Env m) = M.lookup i m

insertEnv :: Ident -> Value -> Env -> Env
insertEnv i v (Env m) = Env $ M.insert i v m

-- ----------------------------------------
--
-- the monad stack: Reader + Error
-- in this case the stacking order doesn't matter

newtype Result a
  = RT { runRT :: ReaderT Env (ExceptT EvalError Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadError  EvalError
           )

runResult :: Result a -> Env -> Either EvalError a
runResult m env
--  = _hole m
--  = _hole (runRT m)
--  = _hole (runReaderT (runRT m))
--  = _hole (runReaderT (runRT m) env)
--  = _hole (runErrorT (runReaderT (runRT m) env))
    = runIdentity (runExceptT (runReaderT (runRT m) env))

-- reverse order compared to Result type

-- ----------------------------------------
-- error handling
  
data EvalError
  = FreeVar String
  | NotImpl String
  | ValErr  String Value 
  | Div0
  | OtherErr String
  deriving (Show)

instance Pretty EvalError where
  pretty (FreeVar i)  = "free variable " ++ show i ++ " in expression"
  pretty (NotImpl n)  = n ++ " not implemented"
  pretty (ValErr e g) = e ++ " value expected, but got: " ++ pretty g
  pretty Div0         = "divide by zero"
  pretty (OtherErr s) = s
  
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

eval' :: Expr -> Either EvalError Value
eval' e = runResult (eval e) emptyEnv

eval :: Expr -> Result Value
eval (BLit b)          = return (B b)
eval (ILit i)          = return (I i)

eval (Var    x)        = do env <- ask
                            case lookupEnv x env of
                             Just v
                               -> return v
                             Nothing
                               -> freeVar x

eval (Unary  op e1)    = do v1  <- eval e1
                            mf1 op v1

eval (Binary op e1 e2) = do v1  <- eval e1
                            v2  <- eval e2
                            mf2 op v1 v2
                            
eval (Cond   c e1 e2)  = do b <- evalBool c
                            if b
                              then eval e1
                              else eval e2

eval (Let x e1 e2)     = do v <- eval e1
                            local (\ env -> insertEnv x v env) (eval e2)

evalBool :: Expr -> Result Bool
evalBool e
  = do r <- eval e
       case r of
        (B b) -> return b
        _     -> boolExpected r

-- ----------------------------------------

type MF1 = Value -> Result Value

mf1 :: Op1 -> MF1
mf1 Not        = op1BB not
mf1 ToInt      = op1BI (toInteger . fromEnum)
mf1 UPlus      = op1II id
mf1 UMinus     = op1II (0 -)
mf1 Signum     = op1II signum
mf1 UPlusMinus = \ _ -> notImpl "+/-"
  
op1BB :: (Bool -> Bool) -> MF1
op1BB op (B b) = return $ B (op b)
op1BB _  v     = boolExpected v

op1II :: (Integer -> Integer) -> MF1
op1II op (I i) = return (I (op i))
op1II _  v     = intExpected v

op1BI :: (Bool -> Integer) -> MF1
op1BI op (B b) = return (I (op b))
op1BI _  v     = boolExpected v

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
op2BBB _  v1     v2
  | not (isB v1)        = boolExpected v1
  | otherwise           = boolExpected v2

op2III :: (Integer -> Integer -> Integer) -> MF2
op2III op (I i1) (I i2) = return (I (i1 `op` i2))
op2III _  v1      v2
  | not (isI v1)        = intExpected v1
  | otherwise           = intExpected v2

op2IIB :: (Integer -> Integer -> Bool) -> MF2
op2IIB op (I i1) (I i2) = return (B (i1 `op` i2))
op2IIB _  v1      v2
  | not (isI v1)        = intExpected v1
  | otherwise           = intExpected v2

divIII :: (Integer -> Integer -> Integer) -> MF2
divIII op (I x) (I y)
  | y == 0              = div0
  | otherwise           = return (I (x  `op` y))
divIII _  v1      v2
  | not (isI v1)        = intExpected v1
  | otherwise           = intExpected v2

-- ----------------------------------------
