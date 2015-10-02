{-# LANGUAGE FlexibleContexts #-}

module Data.Expr.Proposition.Parse
       (parseProp, parse', tp)
       where

import           Data.Pretty

import           Data.Expr.Proposition.Types
import           Data.Expr.Proposition.Constr

import           Control.Applicative ((<$>), (<*))
import           Data.Functor.Identity

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- ----------------------------------------

-- just for testing
tp :: String -> Either String String
tp = either (Left . show) (Right . pp) . parse'

parseProp :: String -> Maybe Expr
parseProp
  = either (const Nothing) Just . parse'

parse' :: String -> Either ParseError Expr
parse' = parse expr0 ""

-- ----------------------------------------

type Parser a = Parsec String () a

expr0, expr, prim :: Parser Expr
           
expr0 = (whiteSpace >> expr) <* eof

expr
    = buildExpressionParser table prim
    where
      table   = [ [ binex "=>"  (.=>.)  AssocLeft
                  , binex "<=>" (.<=>.) AssocLeft
                  , binex "<+>" (.<+>.) AssocLeft
                  ]
                , [ binex "&&"  (.&&.)  AssocRight
                  ]
                , [ binex "||"  (.||.)  AssocRight
                  ]
                ]
      binex name fun assoc
          = Infix  (reservedOp name  >> return fun) assoc

prim
    = parens expr
      <|> (reserved "true"  >> return true)
      <|> (reserved "false" >> return false)
      <|> (reserved "not"    >> (not'    <$> prim))
      <|> (var <$> identifier)

-- ----------------------------------------
-- The lexer

propDef :: P.LanguageDef st
propDef
    = emptyDef
      { P.reservedNames
            = [ "not" ]
      , P.reservedOpNames
          = [ "&&", "||", "=>", "<=>", "</>"
            ]
      }

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser propDef

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

-- ----------------------------------------
