{-# LANGUAGE DefaultSignatures     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Pretty where

import Data.List (intercalate)

class Pretty a where
  pretty :: a -> String

  default pretty :: (Show a) => a -> String
  pretty = show

-- just a shortcut alias for interactive testing
pp :: (Pretty a) => a -> String
pp = pretty

instance Pretty Bool where
  pretty False = "false"
  pretty True  = "true"

instance Pretty Int
instance Pretty Integer
instance Pretty Double

instance (Pretty a) => Pretty [a] where
  pretty [] = "[]"
  pretty xs = "[" ++ intercalate ", " (map pretty xs) ++ "]"

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = either pretty pretty

-- ----------------------------------------
