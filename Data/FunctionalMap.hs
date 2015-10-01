module Data.FunctionalMap
where

import Prelude (Maybe(..), Eq(..), foldl)
import Control.Monad

-- ----------------------------------------

type Map k v = k -> Maybe v

empty :: Map k v
empty = \ _k' -> Nothing

insert :: Eq k => k -> v -> Map k v -> Map k v
insert k v m
  = \ k' -> if k' == k
            then Just v
            else m k'

delete :: Eq k => k -> Map k v -> Map k v
delete k m
  = \ k' -> if k' == k
            then Nothing
            else m k'

lookup :: k -> Map k v -> Maybe v
lookup k m = m k

fromList :: Eq k => [(k, v)] -> Map k v
fromList
  = foldl (\ m (k, v) -> insert k v m) empty

-- direct versions of union and unionWith
union' :: Eq k =>
          Map k v -> Map k v -> Map k v
union' m1 m2
  = \ k' -> case lookup k' m1 of
             Nothing -> lookup k' m2
             v       -> v

unionWith' :: Eq k =>
              (v -> v -> v) ->
              Map k v -> Map k v -> Map k v
unionWith' f m1 m2
  = \ k' -> case (lookup k' m1, lookup k' m2) of
             (Nothing, v2     ) -> v2
             (v1,      Nothing) -> v1
             (Just v1, Just v2) -> Just (f v1 v2)

-- monadic versions of union and unionWith
union :: Eq k =>
         Map k v -> Map k v -> Map k v
union m1 m2
  = \ k' -> lookup k' m1
            `mplus`
            lookup k' m2

unionWith :: Eq k =>
             (v -> v -> v) ->
             Map k v -> Map k v -> Map k v
unionWith f m1 m2
  = \ k' -> ( do v1 <- lookup k' m1
                 ( do v2 <- lookup k' m2
                      return (f v1 v2) )
                   `mplus`
                   return v1
            )
            `mplus`
            lookup k' m2

-- ----------------------------------------
