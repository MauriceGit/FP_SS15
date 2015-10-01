module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-- Wir Currying die Listen aneinander, statt sie direkt zu konkatenieren!

-- ----------------------------------------

-- Wir simulieren Listenkonkatenation durch Currying!!!!! Im Speicher. Von Vorne!!!
fromList        :: [a] -> List a
fromList l      = \ x -> l ++ x  

-- Hier kommt dann die Riesenfunktion rein, wo vorne noch 1 Parameter fehlt. 
toList          :: List a -> [a]
toList l        = l []

empty           :: List a
empty           = \ xs -> xs

singleton       :: a -> List a
singleton e     = fromList [e]

-- (:) for functional lists
cons            :: a -> List a -> List a
cons e l        = append (singleton e) l 
-- cons e l        = \ x -> fromList (l [e])



-- P.length (toList (P.foldl append empty (P.map (fromList.(:[])) [1..50000])))



-- dual to cons
snoc            :: List a -> a -> List a
snoc l e        = append l (singleton e)

-- (++) for functional lists
append          :: List a -> List a -> List a
append l1 l2    =  \ xs -> l1 (l2 xs)
-- append l1 l2    = l1 . l2
-- append          = (.)

-- like concat for normal lists: foldr (++) []
concat          :: [List a] -> List a
concat l        = P.foldr append empty l

-- like map for normal lists: foldr ((:) . f) []
map             :: (a -> b) -> List a -> List b
map f			= foldr (cons . f) empty
-- map f           = foldr (\ x xs -> append (fromList [(f x)]) xs) empty 

-- foldr with foldr for normal lists
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n l     = P.foldr op n (toList l)

-- head, tail, null
head            :: List a -> a
head            = P.head . toList
-- head l       = P.head (toList l)

tail            :: List a -> List a
-- tail        l   = P.tail . l
tail        l   = \ xs -> P.tail (l xs)

null            :: List a -> Bool
-- null      l      = P.null ( toList l)
null 	 		 = P.null . toList

reverse         :: List a -> List a
reverse       l  = (P.reverse . l)

-- ----------------------------------------
