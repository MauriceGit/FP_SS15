{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Expr.Imperative.EvalIOStateErrorMonad where

import Prelude hiding (lookup)

import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State

import Data.Expr.Imperative.Types
import Data.Expr.Imperative.Constr
import Data.Expr.Imperative.Parse (parseLit)
import Data.Pretty

import           Data.Char (isSpace)
import qualified Data.List as L
import qualified Data.Map  as M

import System.IO

-- ----------------------------------------
-- evaluation of simple imperative programs
-- 
-- this is an example for using the state monad
-- the evaluator is derived out of the reader/error
-- evaluator in ArithmLogic dir

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
--
-- the Value / Error sum type
  
data ResVal a
  = R { resVal :: a}
  | E { resErr :: EvalError }
    deriving (Show)

instance Functor ResVal where
  fmap f (R x) = R (f x)
  fmap _ (E e) = E e

instance Applicative ResVal where
  pure = return
  (<*>) = ap
  
instance Monad ResVal where
  return = R
  R x >>= f = f x
  E e >>= _ = E e

instance MonadError EvalError ResVal where
  throwError = E
  catchError r@(R _) _ = r
  catchError   (E e) f = f e
  
instance (Pretty a) => Pretty (ResVal a) where
  pretty (R x) = pretty x
  pretty (E e) = "error: " ++ pretty e

-- ----------------------------------------

newtype Result a = RT { runResult :: Store -> IO (ResVal a, Store) }

instance Functor Result where
  fmap f m = m >>= return.f 
  
instance Applicative Result where
  pure  = return
  (<*>) = ap

instance Monad Result where
  return x = RT $ \ s -> return (R x,s)

  RT sf >>= f
    = RT $ \ s ->
                do
                    y1 <- sf s
                    case y1 of
                        (R p, _) -> runResult (f p) s
                        (E q, b) -> return (E q, b)

instance MonadError EvalError Result where
  throwError e
    = RT $ \ s -> return (E e, s)
  
  catchError (RT sf) handler
    = RT $ \ st ->
            do (rv, st') <- sf st
               case rv of
                E e
                  -> let RT sf' = handler e
                     in
                      sf' st'
                R _
                  -> return (rv, st')

instance MonadState Store Result where
  get
    = RT $ \ s -> return (R s,s)

  put st
    = RT $ \ s -> return (R (), st)

instance MonadIO Result where
  liftIO ioa = RT $ \ st ->
                    do 
                        a <- ioa
                        return (return a, st)
                        
                              
-- ----------------------------------------
--
-- variable store
  
newtype Store = Store (M.Map Ident Value)
              deriving (Show)

instance Pretty Store where
  pretty (Store m)
    = "{" ++ L.intercalate ", " (map pretty' vars) ++ "}"
    where
      pretty' (i, v) = i ++ " :-> " ++ pretty v
      vars = M.toList m
      
emptyStore :: Store
emptyStore = Store M.empty

lookup :: Ident -> Store -> Maybe Value
lookup i (Store m) = M.lookup i m

insert :: Ident -> Value -> Store -> Store
insert i v (Store m) = Store $ M.insert i v m

-- ----------------------------------------
-- error handling
  
data EvalError
  = UndefVar String
  | NotImpl String
  | ValErr  String Value 
  | Div0
  | NoLValue Expr
  | NoParse
  deriving (Show)

instance Pretty EvalError where
  pretty (UndefVar i)  = "undefined variable " ++ show i ++ " in expression"
  pretty (NotImpl n)  = n ++ " not implemented"
  pretty (ValErr e g) = e ++ " value expected, but got: " ++ pretty g
  pretty Div0         = "divide by zero"
  pretty (NoLValue e) = "no lvalue: " ++ pretty e
  pretty NoParse      = "not a value"
  
boolExpected :: Value -> Result a
boolExpected = throwError . ValErr "Bool"

intExpected :: Value -> Result a
intExpected  = throwError . ValErr "Integer"

notImpl :: String -> Result a
notImpl = throwError . NotImpl

undefVar :: String -> Result a
undefVar = throwError . UndefVar

div0 :: Result a
div0  = throwError Div0

noLValue :: Expr -> Result a
noLValue = throwError . NoLValue

noParse :: Result a
noParse = throwError NoParse

-- ----------------------------------------
--
-- input/output primitives (always boring)

readValue :: String -> Result Value
readValue msg
  = do prompt msg
       l <- getLine'
       getVal (parseLit l)
  where
    prompt "" = return ()
    prompt s  = liftIO $
                do hPutStr stdout s
                   hFlush  stdout
                   
    getLine'
      = do l <- liftIO $ hGetLine stdin
           if all isSpace l
             then getLine'
             else return l
        
    getVal Nothing     = noParse
    getVal (Just e)    = toValue e
    
    toValue (BLit b)   = return (B b)
    toValue (ILit i)   = return (I i)
    toValue _          = noParse

writeValue :: String -> Value -> Result ()
writeValue s v
  = do prompt s
       putLine' (pretty v)
  where
    prompt "" = return ()
    prompt x  = liftIO $ hPutStr stdout x

    putLine' l = liftIO $
                 do hPutStrLn stdout l
                    hFlush stdout
                    
-- ----------------------------------------

eval' :: Expr -> IO (ResVal Value, Store)
eval' e = runResult (eval e) emptyStore

eval :: Expr -> Result Value
eval (BLit b)          = return (B b)
eval (ILit i)          = return (I i)

eval (Var    i)        = readVar i
                         
eval (Unary preOp e)
  | preOp `elem` [PreIncr, PreDecr]
                       = do i <- evalLValue e
                            v <- readVar i
                            r <- mf1 preOp v
                            writeVar i r
                            return r

eval (Unary postOp e)
  | postOp `elem` [PostIncr, PostDecr]
                       = do i <- evalLValue e
                            v <- readVar i
                            r <- mf1 postOp v
                            writeVar i r
                            return v
                        
eval (Unary  op e1)    = do v1  <- eval e1
                            mf1 op v1

eval (Binary Assign lhs rhs)
                       = do i <- evalLValue lhs
                            v <- eval       rhs
                            writeVar i v
                            return v

eval (Binary Seq e1 e2)
                       = do _ <- eval e1
                            eval e2
eval (Binary And e1 e2)
                       = eval (cond e1 e2 false)

eval (Binary Or  e1 e2)
                       = eval (cond e1 true e2)

eval (Binary Impl e1 e2)
                       = eval (cond (not' e1) true e2)

eval (Binary op e1 e2)
  | isStrict op        = do v1 <- eval e1
                            v2 <- eval e2
                            mf2 op v1 v2

eval (Binary op _ _)   = notImpl ("operator " ++ pretty op)
                         
eval (Cond   c e1 e2)  = do b <- evalBool c
                            if b
                              then eval e1
                              else eval e2

eval e@(While c body)   = do b <- evalBool c
                             if b
                               then do _ <- eval body
                                       eval e
                               else return (B b)

eval (Read msg)         = readValue msg
eval (Write msg e)      = do undefined
                          
evalBool :: Expr -> Result Bool
evalBool e
  = do r <- eval e
       case r of
        (B b) -> return b
        _     -> boolExpected r

evalLValue :: Expr -> Result Ident
evalLValue (Var i) = return i
evalLValue e       = noLValue e

writeVar :: Ident -> Value -> Result ()
writeVar i v
  = do st <- get
       put (insert i v st)

readVar :: Ident -> Result Value
readVar i
  = do st <- get
       case lookup i st of
        Nothing
          -> undefVar i
        Just v
          -> return v

-- ----------------------------------------

type MF1 = Value -> Result Value

mf1 :: Op1 -> MF1
mf1 Not        = op1BB not
mf1 ToInt      = op1BI (toInteger . fromEnum)
mf1 UPlus      = op1II id
mf1 UMinus     = op1II (0 -)
mf1 Signum     = op1II signum
mf1 PreIncr    = flip (mf2 Plus ) (I 1)
mf1 PreDecr    = flip (mf2 Minus) (I 1)
mf1 PostIncr   = flip (mf2 Plus ) (I 1)
mf1 PostDecr   = flip (mf2 Minus) (I 1)
-- mf1 op         = \ _ -> notImpl (show op)
  
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

isStrict :: Op2 -> Bool
isStrict op
  = op `elem`
    [ Xor, Equiv
    , Plus, Minus, Mult, Div, Mod
    , Eq, Neq, Gr, Ge, Ls, Le
    ]
    
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
mf2 op        = \ _ _ -> notImpl (show op)

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
