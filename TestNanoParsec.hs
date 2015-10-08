{- | this module is derived from the github source at

 "https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter3/parsec.hs"

 which is a part of the excellent tutorial
 Write You a Haskell ("http://dev.stephendiehl.com/fun/")
 authored by Stephen Diehl
-}

-- ----------------------------------------

module TestNanoParsec where

import Data.Char           (isSpace)
import Control.Monad       (when)
import Control.Applicative ((<$>), (<|>))

import Text.NanoParsec

-- ----------------------------------------

{- The grammar
              
 number  = [ "-" ] digit { digit }
 digit   = "0" | "1" | ... | "8" | "9"
 expr    = term { addop term }
 term    = factor { mulop factor }
 factor  = "(" expr ")" | number
 addop   = "+" | "-"
 mulop   = "*"

-}
 
-- ----------------------------------------
-- the abstract syntax
              
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Integer
  deriving Show

-- ----------------------------------------
-- the mini evaluator

eval :: Expr -> Integer
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

-- ----------------------------------------
-- the expr parser combinators

-- parse an integer literal
int :: Parser Expr
int -- = do
        -- s <- number
        -- return (Lit (read s))
        
    -- = number >>= return . Lit . read
    = token ((Lit . read) <$> number)

expr :: Parser Expr
expr = chainl1 (token term) (token addop)

expr' :: Parser Expr -- string -> either ...
expr' = spaces >> expr -- Leerraum überlesen, dann expr aufrufen

term :: Parser Expr
term = chainl1 (token factor) (token mulop)

factor :: Parser Expr
factor =  parens expr <|> int

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp s op = string s >> return op

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

-- ----------------------------------------
-- the main prog

parse :: String -> Either String Expr
parse s = runParser expr' s

main :: IO ()
main = do
  putStr "calc> "
  ex <- getLine
  if ex == "."
    then return ()
    else do when (not (all isSpace ex)) $
              case parse ex of
              Left err -> putStrLn err
              Right e  -> print (eval e)
            main
            
-- ----------------------------------------

