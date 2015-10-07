{-# LANGUAGE FlexibleContexts #-}

module Data.Expr.Imperative.Parse
       (parseExpr, parseLit, parse', tp)
       where

import           Data.Expr.Imperative.Constr
import           Data.Expr.Imperative.Types
import           Data.Pretty

import           Control.Applicative ((<$>), (<*))
import           Data.Functor.Identity

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- ----------------------------------------

tp :: String -> Either String String
tp = either (Left . show) (Right . pretty) . parse'

parseExpr :: String -> Maybe Expr
parseExpr
  = either (const Nothing) Just . parse'
    
parse' :: String -> Either ParseError Expr
parse' = parse expr0 ""

parseLit :: String -> Maybe Expr
parseLit
  = either (const Nothing) Just . parse lit0 ""
    
-- ----------------------------------------

type Parser a = Parsec String () a

-- The parser
expr0
  , expr
  , prim
  , condExpr
  , whileExpr
  , writeExpr
  , lit0, lit :: Parser Expr

lit0 = (whiteSpace >> lit) <* eof

expr0 = (whiteSpace >> expr) <* eof

expr
    = buildExpressionParser table prim
    where
      table   = [ [ postfix "++"     postIncr
                  , postfix "--"     postDecr
                  ]
                , [ prefix  "++"     preIncr
                  , prefix  "--"     preDecr
                  , prefix  "+"      uplus
                  , prefix  "-"      neg'
                  , prefix' "not"    not'
                  , prefix' "ord"    ord'
                  , prefix' "signum" signum'
                  ]
                , [ binex "*"   (.*.)   AssocLeft
                  , binex "/"   (./.)   AssocLeft
                  , binex "%"   (./.)   AssocLeft
                  ]
                , [ binex "+"   (.+.)   AssocLeft
                  , binex "-"   (.-.)   AssocLeft
                  ]
                , [ binex "<"   (.<.)   AssocNone
                  , binex "<="  (.<=.)  AssocNone
                  , binex ">"   (.>.)   AssocNone
                  , binex ">="  (.>=.)  AssocNone
                  , binex "=="  (.==.)  AssocNone
                  , binex "/="  (./=.)  AssocNone

                  , binex "=>"  (.=>.)  AssocLeft
                  , binex "<=>" (.<=>.) AssocLeft
                  , binex "<+>" (.<+>.) AssocLeft
                  ]
                , [ binex "&&"  (.&&.)  AssocRight
                  ]
                , [ binex "||"  (.||.)  AssocRight
                  ]
                , [ binex ":="  (.:=.)  AssocLeft
                  ]
                , [ binex ","   (.>>.)  AssocRight
                  ]
                ]
      binex name fun assoc
          = Infix  (reservedOp name  >> return fun) assoc
      prefix name fun
          = Prefix  (reservedOp name >> return fun)
      prefix' name fun
          = Prefix  (reserved name   >> return fun)
      postfix name fun
          = Postfix (reservedOp name >> return fun)

prim
    = parens expr
      <|> (reserved "true"   >> return true)
      <|> (reserved "false"  >> return false)
      <|> condExpr
      <|> whileExpr
      <|> (mkIntegerLit <$> (natural <* whiteSpace))
      <|> (var <$> identifier)
      <|> (reserved "read"   >> (read' <$> option "" stringLit))
      <|> writeExpr

lit
  = (    reserved "true"   >> return true)
    <|> (reserved "false"  >> return false)
    <|> (mkIntegerLit <$> (integer <* whiteSpace))

condExpr
  = do e1 <- reserved "if"   >> expr
       e2 <- reserved "then" >> expr
       e3 <- reserved "else" >> expr <* reserved "fi" 
       return $ cond e1 e2 e3

whileExpr
  = do e1 <- reserved "while" >> expr
       e2 <- reserved "do"    >> expr <* reserved "done"
       return $ while e1 e2

writeExpr
  = do msg <- reserved "write" >> option "" stringLit
       e   <- prim
       return $ write' msg e

-- ----------------------------------------
-- The lexer

arithmLogicDef :: P.LanguageDef st
arithmLogicDef
    = emptyDef
      { P.reservedNames
            = [ "if", "then", "else", "fi"
              , "while", "do", "done"
              , "not", "ord", "signum"
              , "read", "write"
              ]
      , P.reservedOpNames
          = [ ":=", ","
            , "&&", "||", "=>", "<=>", "</>"
            , "+", "-", "*", "/", "%"
            , "==", "/=", ">", ">=", "<", "<="
            , "++", "--"
            ]
      }

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser arithmLogicDef

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

natural :: Parser Integer
natural = P.natural lexer

integer :: Parser Integer
integer = P.integer lexer

stringLit :: Parser String
stringLit = P.stringLiteral lexer

-- ----------------------------------------
