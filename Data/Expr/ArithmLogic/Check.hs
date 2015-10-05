{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Expr.ArithmLogic.Check where

import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as M

import Data.Expr.ArithmLogic.Types
import Data.Pretty

-- ----------------------------------------
-- simple static check of expressions

-- ----------------------------------------

data Type
  = TBool
  | TInt
  deriving (Eq, Show)
           
instance Pretty Type where
  pretty TBool = "Bool"
  pretty TInt  = "Integer"

-- ----------------------------------------

data StaticError
  = TypeError Type Type
  | SameTypeErr Type Type
  | FreeVar   String
  deriving (Show)

instance Pretty StaticError where
  pretty (TypeError ex gt)   = "type "
                               ++ pretty ex ++
                               " expected, but got "
                               ++ pretty gt
  pretty (SameTypeErr t1 t2) = "equal types expected, but got "
                               ++ pretty t1 ++
                               " and "
                               ++ pretty t2 
  pretty (FreeVar x)         = "free variable "
                               ++ show x ++
                               " found"
  
-- ----------------------------------------

data CheckVal a
  = OK a
  | Err StaticError
  deriving (Show)

instance Functor CheckVal where
  fmap f (OK x)  = OK (f x)
  fmap _ (Err e) = Err e

instance Applicative CheckVal where
  pure = return
  (<*>) = ap
  
instance Monad CheckVal where
  return = OK
  OK x  >>= f = f x
  Err e >>= _ = Err e

instance MonadError StaticError CheckVal where
  throwError = Err
  catchError r@(OK _)  _ = r
  catchError   (Err e) f = f e
  
instance (Pretty a) => Pretty (CheckVal a) where
  pretty (OK x)  = pretty x
  pretty (Err e) = "error: " ++ pretty e
  
-- ----------------------------------------

type Env = Map Ident Type

newtype Check a = CC { unCC :: Env -> CheckVal a }

instance Functor Check where
  fmap f (CC ef) = CC $ \ env -> fmap f (ef env)

instance Applicative Check where
  pure = return
  (<*>) = ap

instance Monad Check where
  return x      = CC $ \ _env -> return x
  (CC ef) >>= f = CC $ \  env -> do x <- ef env
                                    let CC ef' = f x
                                    ef' env
  
instance MonadError StaticError Check where
  throwError e
    = CC $ \ _env -> throwError e

  catchError (CC ef) handler
    = CC $ \ env -> catchError (ef env)
                    (\ e -> let CC ef' = handler e in ef' env)

instance MonadReader Env Check where
  ask
    = CC $ \ env -> return env

  local f (CC ef)
    = CC $ \ env -> ef (f env)
  
-- ----------------------------------------

freeVar :: String -> Check a
freeVar = throwError . FreeVar

boolExpected :: Type -> Check a
boolExpected = throwError . TypeError TBool

intExpected :: Type -> Check a
intExpected  = throwError . TypeError TInt

sameTypeExpected :: Type -> Type -> Check a
sameTypeExpected t1 = throwError . SameTypeErr t1

-- ----------------------------------------

check' :: Expr -> CheckVal Type
check' e = (unCC . check) e M.empty -- start with empty env

check :: Expr -> Check Type
check (BLit _) = return TBool
check (ILit _) = return TInt

check (Var x)    = do env <- ask
                      case M.lookup x env of
                       Just t
                         -> return t
                       Nothing
                         -> freeVar x
check (Unary op e1)
                 = do t1 <- check e1
                      tf1 op t1

check (Binary op e1 e2)
                 = do t1 <- check e1
                      t2 <- check e2
                      tf2 op t1 t2

check (Cond c e1 e2)
                 = do ct <- check c
                      _ <- op1BB ct
                      t1 <- check e1
                      t2 <- check e2
                      op2EEE t1 t2

check (Let x e1 e2)
                 = do t <- check e1
                      local (\ env -> M.insert x t env) (check e2)
                         
-- ----------------------------------------

type TF1 = Type -> Check Type

tf1 :: Op1 -> TF1
tf1 Not        = op1BB
tf1 ToInt      = op1BI
tf1 UPlus      = op1II
tf1 UMinus     = op1II
tf1 Signum     = op1II
tf1 UPlusMinus = op1II
  
op1BB :: TF1
op1BB TBool = return TBool
op1BB t     = boolExpected t

op1II :: TF1
op1II TInt = return TInt
op1II t    = intExpected t

op1BI :: TF1
op1BI TBool = return TInt
op1BI t     = boolExpected t

-- ----------------------------------------

type TF2 = Type -> Type -> Check Type

tf2 :: Op2 -> TF2
tf2 And       = op2BBB
tf2 Or        = op2BBB
tf2 Impl      = op2BBB
tf2 Xor       = op2BBB
tf2 Equiv     = op2BBB
tf2 Plus      = op2III
tf2 Minus     = op2III
tf2 Mult      = op2III
tf2 Div       = op2III
tf2 Mod       = op2III
tf2 Eq        = op2IIB
tf2 Neq       = op2IIB
tf2 Ge        = op2IIB
tf2 Gr        = op2IIB
tf2 Le        = op2IIB
tf2 Ls        = op2IIB
tf2 PlusMinus = op2III
tf2 Alt       = op2EEE
  
op2BBB :: TF2
op2BBB TBool TBool = return TBool
op2BBB TBool t2    = boolExpected t2
op2BBB t1    _     = boolExpected t1

op2III :: TF2
op2III TInt  TInt  = return TInt
op2III TInt  t2    = intExpected t2
op2III t1    _     = intExpected t1

op2IIB :: TF2
op2IIB TInt  TInt  = return TBool
op2IIB TInt  t2    = intExpected t2
op2IIB t1    _     = intExpected t1

op2EEE :: TF2
op2EEE t1    t2
  | t1 == t2       = return t1
  | otherwise      = sameTypeExpected t1 t2

-- ----------------------------------------
