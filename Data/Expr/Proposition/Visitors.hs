-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------

type Idents = Set Ident

freeVarsVisitor :: Visitor Idents
freeVarsVisitor = V {vLit = const S.empty, vVar = S.singleton, vUnary = (\ f -> id), vBinary = (\ f -> S.union)}

freeVars :: Expr -> Idents
freeVars = visit freeVarsVisitor

-- Testfall:
-- freeVars (either (error.show) id  (parse' "a && true || b => c"))
    
type VarEnv = [(Ident, Expr)]

substVarsVisitor :: VarEnv -> Visitor Expr
substVarsVisitor env = V {vLit = Lit . id, vVar = (\ i -> maybe (Var i) id (lookup i env)), vUnary = Unary . id, vBinary = Binary . id} 
-- Binary . id  <===>  (\ f e1 e2 -> Binary f e1 e2)
-- Das Gleiche gilt für Unary und Lit

substVars :: VarEnv -> Expr -> Expr
substVars env = visit $ substVarsVisitor env

-- Testfall:
-- substVars [("a", Lit False)] ( either (error.show) id (parse' "a && b"))

evalVisitor :: Visitor Bool
evalVisitor = V {vLit = id, vVar = (\ x -> error "There shouldn't be variables any more..."), vUnary = (\ f e -> (mf1 f) e), vBinary = (\ f e1 e2 -> (mf2 f) e1 e2)}

eval :: Expr -> Bool
eval = visit evalVisitor

-- Testfall:
-- eval (either (error.show) id  (parse' "false && true && false => true"))
-- eval (either (error.show) id  (parse' "false && true || true => false"))

-- ----------------------------------------
