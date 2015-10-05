{-# LANGUAGE FlexibleContexts #-}

module Data.Expr.ArithmLogic.Parse
       (parseExpr, parse', tp)
       where

import           Data.Expr.ArithmLogic.Constr
import           Data.Expr.ArithmLogic.Types
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

-- ----------------------------------------

type Parser a = Parsec String () a

-- The parser
expr0
  , expr
  , prim
  , condExpr
  , letExpr :: Parser Expr
           
expr0 = (whiteSpace >> expr) <* eof

expr
    = buildExpressionParser table prim
    where
      table   = [ [ binex "*"   (.*.)   AssocLeft
                  , binex "/"   (./.)   AssocLeft
                  , binex "%"   (./.)   AssocLeft
                  ]
                , [ binex "+"   (.+.)   AssocLeft
                  , binex "-"   (.-.)   AssocLeft
                  , binex "+/-" (.+/-.) AssocLeft
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
                  , binex "|||" (.|||.) AssocLeft
                  ]
                ]
      binex name fun assoc
          = Infix  (reservedOp name  >> return fun) assoc
      {-
      prefix name fun
          = Prefix  (reservedOp name >> return fun)
      prefix' name fun
          = Prefix  (reserved name   >> return fun)
      postfix name fun
          = Postfix (reservedOp name >> return fun)
      -- -}

prim
    = parens expr
      <|> (reserved "true"   >> return true)
      <|> (reserved "false"  >> return false)
      <|> (reserved "signum" >> (signum'    <$> prim))
      <|> (reserved "not"    >> (not'       <$> prim))
      <|> (reserved "ord"    >> (ord'       <$> prim))
      <|> (reservedOp "+/-"  >> (uplusMinus <$> prim))
      <|> (reservedOp "+"    >> (uplus      <$> prim))
      <|> (reservedOp "-"    >> (neg'       <$> prim))
      <|> condExpr
      <|> letExpr
      <|> (mkIntegerLit <$> integer)
      <|> (var <$> identifier)

condExpr
    = do e1 <- reserved "if"   >> expr
         e2 <- reserved "then" >> expr
         e3 <- reserved "else" >> expr
         return $ cond e1 e2 e3

letExpr
    = do ident <- reserved   "let" >> identifier
         e1    <- reservedOp "="   >> expr
         e2    <- reserved   "in"  >> expr
         return $ let' ident e1 e2

-- ----------------------------------------
-- The lexer

arithmLogicDef :: P.LanguageDef st
arithmLogicDef
    = emptyDef
      { P.reservedNames
            = ["if", "then", "else", "let", "in", "not", "ord", "signum"]
      , P.reservedOpNames
          = [ "=", "\\", "->"
            , "&&", "||", "=>", "<=>", "</>"
            , "+", "-", "*", "/", "%"
            , "==", "/=", ">", ">=", "<", "<="
            , "+/-", "|||"
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

integer :: Parser Integer
integer = P.integer lexer

-- ----------------------------------------
