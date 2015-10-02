module ProofProposition where

import           Data.Expr.Proposition
import qualified Data.Expr.Proposition.Visitors as V
import           Data.Pretty

-- ----------------------------------------
-- test expressions

exs :: [Expr]
ex1, ex2, ex3, ex4, ex5, ex6 :: Expr
exs@[ex1, ex2, ex3, ex4, ex5, ex6]
  = [ x1 .&&. not' x2
    , (x1 .||. x2) .||. x3
    , x1 .=>. x2
    , x1 .<+>. x2
    , x1 .<=>. x2
    , not' . not' . not' . not' . not' $ ex1
    ]

vars :: [Expr]
x1, x2, x3, x4 :: Expr
vars@[x1, x2, x3, x4]
  = map var ["x1", "x2", "x3", "x4"]

propEquiv :: Expr
propEquiv = (x1 .<=>. x2) .<=>. (x1 .&&. x2 .||. not' x1 .&&. not' x2)

propModusPonens :: Expr
propModusPonens = ((x1 .=>. x2) .&&. x1) .=>. x2

t10, t11, t12 :: Expr
t10 = substVars [("x2", false)] ex6
t11 = V.substVars [("x1", true)] ex6
t12 = V.substVars [("x2", false)] t11

p1 :: String
p1  = proof propModusPonens

toExpr :: String -> Expr
toExpr = maybe (error "no parse") id . parseProp

tt0, tt1, tt2 :: String
tt0 = pretty propModusPonens
tt1 = pretty $ toExpr "x && y || z"
tt2 = pretty $ toExpr "x x x"

-- ----------------------------------------
