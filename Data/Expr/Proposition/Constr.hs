-- | smart constructors for boolean expressions

module Data.Expr.Proposition.Constr where

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- (smart) construtors

infixr 2 .||.   -- or
infixr 3 .&&.   -- and
infix  4 .=>.   -- implication
infix  4 .<=>.  -- equivalence
infix  4 .<+>.  -- exclusive or

false, true :: Expr
true   = Lit True
false  = Lit False

bool :: Bool -> Expr
bool = Lit

var :: String -> Expr
var    = Var

binary :: Op2 -> Expr -> Expr -> Expr
binary = Binary

(.&&.), (.||.), (.=>.), (.<=>.), (.<+>.) :: Expr -> Expr -> Expr

(.&&.)  = binary And
(.||.)  = binary Or
(.=>.)  = binary Impl
(.<+>.) = binary Xor
(.<=>.) = binary Equiv

not' :: Expr -> Expr
not' = Unary Not

-- ----------------------------------------
