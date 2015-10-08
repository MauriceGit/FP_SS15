{- | this module is derived from the github source at

 "https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter3/parsec.hs"

 which is a part of the excellent tutorial
 Write You a Haskell ("http://dev.stephendiehl.com/fun/")
 authored by Stephen Diehl
-}

-- ----------------------------------------

module Text.NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

-- ----------------------------------------
newtype Parser a
  = Parser { runP :: String -> Either String (a, String) }

runParser :: Parser a -> String -> Either String a
runParser p s =
  case runP (p <* eof) s of
    Left  err      -> Left ("Syntaz error: " ++ err)
    Right (res, _) -> Right res

-- ----------------------------------------
--
-- the basic parsers

item :: Parser Char
item = Parser $
       \s ->
        case s of
         []     -> Left "end of input"
         (c:cs) -> Right (c, cs)

unit :: a -> Parser a
unit a
  = Parser (\s -> Right (a, s))

failure :: String -> Parser a
failure msg
  = Parser (\ _s -> Left msg)

eof :: Parser ()
eof = Parser $
      \ s ->
       case s of
         [] -> Right ((), s)
         _  -> Left  ("eof expected, but seen: " ++ show s)

-- ----------------------------------------
-- sequencing and choice

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f
  = Parser $
    \ s ->
     case runP p s of
      Left err      -> Left err
      Right (x, s') -> runP (f x) s'

option :: Parser a -> Parser a -> Parser a
option  p q
  = Parser $
    \s ->
     case runP p s of
      Left _ -> runP q s
      res    -> res

-- ----------------------------------------
-- the standard interfaces
    
instance Functor Parser where
  fmap f (Parser sf)
    = Parser $
      \ s ->
       fmap (\ (x, s') -> (f x, s')) (sf s)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance Alternative Parser where
  empty = failure "empty"
  (<|>) = option

-- ----------------------------------------
-- basic combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy p
  = do c <- item
       if p c
         then return c
         else failure ("unexpected char " ++ show c)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

char :: Char -> Parser Char
char c = satisfy (c ==)

-- many is already defined in class Alternative
-- with the same semantics as many'

many' :: Parser a -> Parser [a]
many' p
  = do x  <- p
       xs <- many p
       return (x : xs)
    <|>
    return []
    
-- parse list of left assoc expr/operators without left recursion
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op
  = do x <- p
       rest x
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y)
             )
             <|> return x

digit :: Parser Char
digit = satisfy isDigit

string :: String -> Parser String
string []     = return []
string (c:cs) = do _ <- char c
                   _ <- string cs
                   return (c:cs)

number :: Parser String
number = do
  s  <- string "-"
        <|>
        return []
  cs <- some digit
  return (s ++ cs)

-- ----------------------------------------
-- token parsers and whitespace handling

spaces :: Parser String
spaces = many $ satisfy isSpace

-- remove trailing whitespace
token :: Parser a -> Parser a
token p = do a <- p
             _ <- spaces
             return a

reserved :: String -> Parser String
reserved s = token (string s)

parens :: Parser a -> Parser a
parens m = do _ <- reserved "("
              n <- m
              _ <- reserved ")"
              return n

-- ----------------------------------------
