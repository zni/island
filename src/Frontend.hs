-- ----------------------------------------------
-- Author  : Matthew Godshall
-- Date    : 2011 May 13 18:16:03
-- Updated : 2011 May 15 18:58:34
-- 
-- Parser for the interpreter.
-- ----------------------------------------------
module Frontend where

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Combinator
import Text.Parsec.Prim (many)

import Types

def = emptyDef{ commentStart = "{-"
              , commentEnd   = "-}"
              , identStart   = letter
              , identLetter = alphaNum
              , opStart      = oneOf "+-*%/="
              , reservedOpNames = ["+", "-", "*", "%",
                                   "/", "=", "++", "--"]
              , reservedNames = ["if", "then", "else", "let",
                                 "letrec", "in", "val", "proc"]
              }


TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , integer = m_integer
           , symbol = m_symbol
           , whiteSpace = m_whiteSpace } = makeTokenParser def


expParser :: Parser Exp
expParser = buildExpressionParser table term
          <|> letExp      
          <|> letRecExp
          <|> ifExp
          <|> proc
          <|> app
          <?> "expression"


table = [ [Prefix (m_reservedOp "-" >> return (UnExp Minus))]
        , [Infix (m_reservedOp "*" >> return (BinExp Mult)) AssocLeft]
        , [Infix (m_reservedOp "/" >> return (BinExp Div)) AssocLeft]
        , [Infix (m_reservedOp "%" >> return (BinExp Mod)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (BinExp Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (BinExp Sub)) AssocLeft]
        ]


-- Expression terms
term = try (m_parens expParser)
    <|> app
    <|> fmap Var m_identifier
    <|> fmap Lit m_integer


-- Parse 'let' expression.
letExp :: Parser Exp
letExp = do try $ m_reserved "let"
            b <- many1 ( do { m_reserved "val"
                            ; i <- m_identifier
                            ; m_reservedOp "="
                            ; e <- expParser
                            ; return (i,e)
                            } )
            m_reserved "in"
            e <- expParser
            return (LetExp b e)


-- Parse 'letrec' expression.
letRecExp :: Parser Exp
letRecExp = do m_reserved "letrec"
               b <- many1 ( do { procName <- m_identifier
                               ; ids <- m_parens $ m_identifier `sepBy` (m_symbol ",")
                               ; m_reservedOp "="
                               ; body <- expParser
                               ; return (procName,ids,body)
                               } )
               m_reserved "in"
               e <- expParser
               return (LetRecExp b e)


-- Parse 'if' expression.
ifExp :: Parser Exp
ifExp = do m_reserved "if"
           p <- expParser
           m_reserved "then"
           t <- expParser
           m_reserved "else"
           e <- expParser
           return (IfExp p t e)


-- Parse 'proc' expression.
proc :: Parser Exp
proc = do m_reserved "proc"
          ids <- m_parens $ m_identifier `sepBy` (m_symbol ",")
          body <- expParser
          return (ProcExp ids body)


-- Parse 'app' expression.
app :: Parser Exp
app = do (op, ops) <- m_parens ( do { op <- expParser
                                    ; ops <- many expParser
                                    ; return (op, ops)
                                    } )
         return (AppExp op ops)
         

mainParser :: Parser Exp
mainParser = m_whiteSpace >> expParser <* eof


lparse   :: String -> IO Exp
lparse f = do { result <- parseFromFile mainParser f 
              ; case result of
                    Left err   -> error $ show err
                    Right expr -> return expr
              }
