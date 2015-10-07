module Test.Imperative
where

import           Data.Expr.Imperative
import qualified Data.Expr.Imperative.EvalStateErrorMonad    as S

import qualified Data.Expr.Imperative.EvalIOStateErrorMonad  as O
{-
import qualified Data.Expr.Imperative.EvalStateErrorTrans    as ST
import qualified Data.Expr.Imperative.EvalIOStateErrorTrans  as OT
-- -}
import           Data.Pretty

import           Data.Maybe

pe :: String -> Expr
pe = fromJust . parseExpr

e1, e2, e3, e4, e5, e6, e7, e8, e9
  , e10, e11, e12, e13, e14
  , e20 :: Expr
                         
e1 = pe "1 + 2 - 3 "
e2 = pe "1 + 2 * 3 / 7"
e3 = pe "(true && false) => true"
e4 = pe " (1 > 2) <=> (2 < 1) "
e5 = pe "if 1 > 2 then 1 else 2 fi"
e6 = pe "x:=1, y:=2"
e7 = pe " 1 + true "
e8 = pe " 1 / 0 "
e9 = pe "x := 5, y := x * x, y + y"
e10 = pe "x:=5, (x ++ - 2) * (x-- + 1)"
e11 = pe "x:=5, x++ - ++x"
e12 = pe "x:= 42, while x > 0 do --x done"
e13 = pe "1 := 1"
e14 = pe "x := read \"x = \", y := read \"y = \", write \"x + y = \" (x + y)"

e20 = pe $ unlines
  [ "x := read \"x = \","
  , "y := read \"y = \","
  , "while x /= y do"
  , "  if x > y then x := x - y else y := y - x fi"
  , "done,"
  , "write \"ggt(x, y) = \" x"
  ]

eval1 :: Expr -> (S.ResVal S.Value, S.Store)
eval1 = S.eval'

pp1 :: Expr -> IO ()
pp1 = putStrLn . pretty . eval1

 {-
eval2 :: Expr -> IO (O.ResVal O.Value, O.Store)
eval2 = O.eval'

pp2 :: Expr -> IO ()
pp2 e = do res <- eval2 e
           putStrLn (pretty res)

eval3 :: Expr -> (Either ST.EvalError ST.Value, ST.Store)
eval3 = ST.eval'

pp3 :: Expr -> IO ()
pp3 = putStrLn . pretty . eval3

eval4 :: Expr -> IO (Either OT.EvalError OT.Value, OT.Store)
eval4 = OT.eval'

pp4 :: Expr -> IO ()
pp4 e = do res <- eval4 e
           putStrLn (pretty res)
-- -}



