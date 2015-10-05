module Test.ArithmLogic
where

import           Data.Expr.ArithmLogic
import qualified Data.Expr.ArithmLogic.Eval                  as S
import qualified Data.Expr.ArithmLogic.EvalMonad             as M
import qualified Data.Expr.ArithmLogic.EvalErrorMonad        as E


import qualified Data.Expr.ArithmLogic.EvalReaderErrorMonad  as R
{--import qualified Data.Expr.ArithmLogic.EvalReaderErrorTrans  as RT
-- import qualified Data.Expr.ArithmLogic.EvalListErrorMonad    as L
import qualified Data.Expr.ArithmLogic.Check                 as C
-- -}
import           Data.Pretty

import           Data.Maybe

pe :: String -> Expr
pe = fromJust . parseExpr

e1, e2, e3, e4, e5, e6, e7, e8, e9
  , e10, e11, e12, e13, e14, e15, e16, e17, e18, e19 :: Expr
                          
e1 = pe "1 + 2 - 3 "
e2 = pe "1 + 2 * 3 / 7"
e3 = pe "(true && false) => true"
e4 = pe " (1 > 2) <=> (2 < 1) "
e5 = pe "if 1 > 2 then 1 else 2"
e6 = pe "x"
e7 = pe " 1 + true "
e8 = pe " 1 / 0 "
e9 = pe "let x = 5 in let y = x * x in y + y"
e10 = pe "(1 +/- 2) * (3 +/- 1)"
e11 = pe "2 / (1 +/- 1)"
e12 = pe "((1 ||| 2) + (3 ||| 5)) * 10"
e13 = pe "let x = 1 in if x > 2 then x else false"
e14 = pe "1 ||| true"
e15 = pe "(1 ||| 2 ||| 3) * (5 ||| 7 ||| 9)"
e16 = pe "1 ||| 2 ||| 3"
e17 = pe "if true ||| false ||| true then 1 else 2"
e18 = pe "if true ||| false ||| true then 1 else false"
e19 = pe "if true ||| false ||| 1    then 1 else false"

eval1 :: Expr -> S.Value
eval1 = S.eval

pp1 :: Expr -> IO ()
pp1 = putStrLn . pretty . eval1

eval2 :: Expr -> M.Result M.Value
eval2 = M.eval

pp2 :: Expr -> IO ()
pp2 = putStrLn . pretty . eval2

eval3 :: Expr -> E.Result E.Value
eval3 = E.eval

pp3 :: Expr -> IO ()
pp3 = putStrLn . pretty . eval3

 {-
eval4 :: Expr -> L.Result L.Value
eval4 = L.eval

pp4 :: Expr -> IO ()
pp4 = putStrLn . pretty . eval4

eval5 :: Expr -> R.ResVal R.Value
eval5 = R.eval'

pp5 :: Expr -> IO ()
pp5 = putStrLn . pretty . eval5

typ :: Expr -> IO ()
typ = putStrLn . pretty . C.check'

eval6 :: Expr -> Either RT.EvalError RT.Value
eval6 = RT.eval'

pp6 :: Expr -> IO ()
pp6 = putStrLn . pretty . eval6
-- -}
