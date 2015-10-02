{-# LANGUAGE DeriveDataTypeable #-}

-- ----------------------------------------

-- | binary tree with values at the leafs (Tip),
-- the branches (Bin) don't contain any further information,
-- the empty tree is represented by a special value Null

module Data.Tree
where

import           Prelude             hiding (foldl, foldr, head, tail, init, last)

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Foldable
import           Data.Monoid

-- ----------------------------------------

data Tree a
    = Null
    | Tip a
    | Bin (Tree a) (Tree a)
      deriving (Show, Data, Typeable)

-- | smart constructor
bin :: Tree a -> Tree a -> Tree a
bin Null r = r
bin l Null = l
bin l r = Bin l r

instance Functor Tree where
  fmap f Null = Null
  fmap f (Tip x) = Tip (f x)
  fmap f (Bin x y) = Bin (fmap f x) (fmap f y)
  
instance Applicative Tree where
  pure  = undefined
  (<*>) = undefined
  
instance Monad Tree where
  return     = undefined
  _    >>= _ = undefined

instance Alternative Tree where
  empty = mzero   -- or Null
  (<|>) = mplus

instance MonadPlus Tree where
  mzero = undefined
  mplus = undefined

instance Monoid (Tree a) where
  mempty  = undefined
  mappend = undefined

-- fold elements like in a list from right to left
instance Foldable Tree where
  foldr f e Null = e
  foldr f e (Tip t) = f t e
  foldr f e (Bin l r) = foldr f (foldr f e r) l

-- ----------------------------------------
-- classical visitor

visitTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
visitTree e tf bf = visit'
  where
    visit' Null = e
    visit' (Tip t) = tf t
    visit' (Bin left right) = bf (visit' left) (visit' right)

-- special visitors

sizeTree :: Tree a -> Int
sizeTree = visitTree 0 (const 1) (+)

minDepth, maxDepth :: Tree a -> Int
minDepth = visitTree 0 (\ t -> 1) (\ x y -> min x y + 1)
maxDepth = visitTree 0 (\ t -> 1) (\ x y -> max x y + 1)

-- ----------------------------------------
-- access functions

viewL :: Tree a -> Maybe (a, Tree a) -- gibt das linkeste Element aus und den 
                                     -- Restbaum ohne das Element (Head, Tail)
viewL Null = Nothing
viewL (Tip x) = Just (x, Null)
viewL (Bin left right) = Just (l, bin q right)
    where 
    Just (l, q) = viewL left

viewR :: Tree a -> Maybe (Tree a, a)-- gibt das rechteste Element aus und den Restbaum ohne das Element (Init + Last)
viewR Null = Nothing
viewR (Tip x) = Just (Null, x)
viewR (Bin left right) = Just (bin left l, r)
    where
    Just (l,r) = viewR right

head :: Tree a -> a
head = maybe (error "head: empty tree") fst . viewL

tail :: Tree a -> Tree a
tail = maybe (error "tail: empty tree") snd . viewL

last :: Tree a -> a
last = maybe (error "last: empty tree") snd . viewR

init :: Tree a -> Tree a
init = maybe (error "init: empty tree") fst . viewR

-- ----------------------------------------
-- conversions to/from lists



-- | runs in O(n) due to the use of (:)
toList :: Tree a -> [a]
toList = foldr (:) []

-- | runs in O(n^2) due to the use of (++)
toListSlow :: Tree a -> [a]
toListSlow = visitTree [] (:[]) (++)

-- | build a balanced tree
--
-- doesn't work for infinite lists

-- strong balancing criterion
fromList :: [a] -> Tree a
fromList [] = Null
fromList (x : []) = Tip x
fromList xs = Bin (fromList l) (fromList r)
    where
        (l, r) = splitAt (div (length xs) 2) xs

-- weak balancing criterion
fromList' :: [a] -> Tree a
fromList' = undefined

-- list to the right
fromList'' :: [a] -> Tree a
fromList'' = foldr (\ x t -> Tip x `bin` t) Null

-- list to the left
fromList''' :: [a] -> Tree a
fromList''' = foldl (\ t x -> t `bin` Tip x) Null

-- runtime differences between fromList, fromList', fromList'', fromList'''?
-- differences in balancing quality?

-- ----------------------------------------
