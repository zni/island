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
                                 "in", "val", "proc"]
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
          -- Parse 'let' expression.
          <|> do m_reserved "let"
                 b <- many1 ( do { m_reserved "val"
                                 ; i <- m_identifier
                                 ; m_reservedOp "="
                                 ; e <- expParser
                                 ; return (i,e)
                                 } )
                 m_reserved "in"
                 e <- expParser
                 return (LetExp b e)
                 
          -- Parse 'if' expression.
          <|> do m_reserved "if"
                 p <- expParser
                 m_reserved "then"
                 t <- expParser
                 m_reserved "else"
                 e <- expParser
                 return (IfExp p t e)
 
          <|> proc
          <|> do (op, ops) <- m_parens ( do { op <- (proc <|> (fmap Var m_identifier))
                                            ; ops <- expParser `sepBy` (m_whiteSpace)
                                            ; return (op, ops)
                                            } )
                 return (AppExp op ops)
          <?> "expression"

table = [ [Prefix (m_reservedOp "-" >> return (UnExp Minus))]
        , [Infix (m_reservedOp "*" >> return (BinExp Mult)) AssocLeft]
        , [Infix (m_reservedOp "/" >> return (BinExp Div)) AssocLeft]
        , [Infix (m_reservedOp "%" >> return (BinExp Mod)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (BinExp Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (BinExp Sub)) AssocLeft]
        ]

term = try $ m_parens expParser
    <|> fmap Var m_identifier
    <|> fmap Lit m_integer

-- Parse 'proc' expression.
proc :: Parser Exp
proc = do m_reserved "proc"
          ids <- m_parens $ m_identifier `sepBy` (m_symbol ",")
          body <- expParser
          return (ProcExp ids body)

mainParser :: Parser Exp
mainParser = m_whiteSpace >> expParser <* eof

lparse   :: String -> IO Exp
lparse f = do { result <- parseFromFile mainParser f 
              ; case result of
                    Left err   -> error $ show err
                    Right expr -> return expr
              }
