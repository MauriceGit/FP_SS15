{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Expr.ArithmLogic.Constr where

import Data.Expr.ArithmLogic.Types

-- ----------------------------------------
--
-- smart constructors

var :: String -> Expr
var = Var

unary :: Op1 -> Expr -> Expr
unary = Unary

binary :: Op2 -> Expr -> Expr -> Expr
binary = Binary

cond :: Expr -> Expr -> Expr -> Expr
cond = Cond

let' :: Ident -> Expr -> Expr -> Expr
let' = Let

mkIntegerLit :: Integer -> Expr
mkIntegerLit = ILit

mkBoolLit :: Bool -> Expr
mkBoolLit = BLit

true :: Expr
true = mkBoolLit True

false :: Expr
false = mkBoolLit False

-- ----------------------------------------
--
-- some generally usefull operators
-- for construction of expressions

-- for arithmetic

infixl 6 .+.
infixl 6 .-.
infixl 7 .*.
infixl 7 ./.
infixl 7 .%.
infixl 6 .+/-.
infixl 2 .|||.

uplus, neg', signum', ord', uplusMinus :: Expr -> Expr

uplus       = unary UPlus
neg'        = unary UMinus
ord'        = unary ToInt
signum'     = unary Signum
uplusMinus  = unary UPlusMinus

(.+.)
  , (.-.)
  , (.*.)
  , (./.)
  , (.%.)
  , (.+/-.)
  , (.|||.) :: Expr -> Expr -> Expr

(.+.)   = binary Plus
(.-.)   = binary Minus
(.*.)   = binary Mult
(./.)   = binary Div
(.%.)   = binary Mod
(.+/-.) = binary PlusMinus
(.|||.) = binary Alt

-- for logical ops

infixr 2 .||.
infixr 3 .&&.
infix  4 .=>.
infixl 4 .<=>.
infixl 4 .<+>.

not' :: Expr -> Expr
not' = unary Not

(.&&.)
  , (.||.)
  , (.=>.)
  , (.<=>.)
  , (.<+>.) :: Expr -> Expr -> Expr

(.&&.)  = binary And
(.||.)  = binary Or
(.=>.)  = binary Impl
(.<+>.) = binary Xor
(.<=>.) = binary Equiv


-- for relational ops

infix 4 .<.
infix 4 .>.
infix 4 .<=.
infix 4 .>=.
infix 4 .==.
infix 4 ./=.

(.<.)
  , (.>.)
  , (.<=.)
  , (.>=.)
  , (.==.)
  , (./=.) :: Expr -> Expr -> Expr

(.<.)   = binary Ls
(.<=.)  = binary Le
(.>=.)  = binary Ge
(.>.)   = binary Gr
(.==.)  = binary Eq
(./=.)  = binary Neq

-- for transformations

isAssoc :: Op2 -> Bool
isAssoc And   = True
isAssoc Or    = True
isAssoc Xor   = True
isAssoc Equiv = True
isAssoc Plus  = True
isAssoc Mult  = True
isAssoc Alt   = True
isAssoc _     = False

isSymmetric :: Op2 -> Bool
isSymmetric And   = True
isSymmetric Or    = True
isSymmetric Xor   = True
isSymmetric Equiv = True
isSymmetric Plus  = True
isSymmetric Mult  = True
isSymmetric Alt   = True
isSymmetric _     = False

-- ----------------------------------------
