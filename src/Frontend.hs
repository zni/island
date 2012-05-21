-- ----------------------------------------------
-- Author  : Matthew Godshall
-- Date    : 2011 May 13 18:16:03
-- 
-- Parser for the interpreter.
-- ----------------------------------------------
module Frontend (lparse) where

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim (many)

import Types

def = emptyDef{ commentStart = "{-"
              , commentEnd   = "-}"
              , identStart   = letter
              , identLetter = alphaNum
              , opStart      = oneOf "+-*%/="
              , reservedOpNames = ["+", "-", "*", "%",
                                   "/", "=", "succ", "pred"]
              , reservedNames = ["if", "then", "else", "let",
                                 "letrec", "in", "val", "proc", "type"]
              }


TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , integer = m_integer
           , symbol = m_symbol
           , whiteSpace = m_whiteSpace } = makeTokenParser def


expParser :: Parser Exp
expParser = try term
          <|> letExp      
          <|> letRecExp
          <|> ifExp
          <|> proc
          <|> app
          <?> "expression"

-- Parse a primitive expression operator.
ops =  (m_reservedOp "+" >> return Add)
   <|> (m_reservedOp "-" >> return Sub)
   <|> (m_reservedOp "%" >> return Mod)
   <|> (m_reservedOp "*" >> return Mult)
   <|> (m_reservedOp "/" >> return Div)
   <|> (m_reservedOp "succ" >> return Succ)
   <|> (m_reservedOp "pred" >> return Pred)
   <?> "operand"

primExp :: Parser Exp
primExp = do
  op <- ops
  exps <- m_parens $ term `sepBy` m_whiteSpace
  if null exps
    then fail "cannot apply a primitive operation to an empty expression list."
    else return $ PrimExp op exps
  

-- Expression terms
term = primExp
    <|> app
    <|> fmap Var m_identifier
    <|> fmap LitInt m_integer
    <?> "term"


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
                               ; ids <- m_parens $ m_identifier `sepBy` m_symbol ","
                               ; m_reservedOp "="
                               ; body <- expParser
                               ; return (procName,ids,body)
                               } )
               m_reserved "in"
               e <- expParser
               return (LetRecExp b e)

typeDec :: Parser Exp
typeDec = do m_reserved "type"
             name <- typeName
             m_reservedOp "="
             constructors <- typeCons `sepBy` m_symbol ","
             return $ TypeDec name constructors

typeName :: Parser String
typeName = do t    <- upper
              rest <- many letter
              m_whiteSpace
              return $ t:rest

typeCons :: Parser TypeCons
typeCons = fmap TypeCons typeName

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
          ids <- m_parens $ m_identifier `sepBy` m_symbol ","
          body <- expParser
          return (ProcExp ids body)


-- Parse 'app' expression.
app :: Parser Exp
app = do (op, ops) <- m_parens (do { op <- expParser
                                   ; ops <- many expParser
                                   ; return (op, ops)
                                   } )
         return (AppExp op ops)

-- FIXME Quick Hack - Some more thought needs to be put into this.
program :: Parser Exp
program = typeDec
       <|> expParser

mainParser :: Parser [Exp]
mainParser = m_whiteSpace >> many1 program <* eof


lparse   :: String -> IO [Exp]
lparse f = do 
  result <- parseFromFile mainParser f 
  case result of
    Left err   -> error $ show err
    Right expr -> return expr

-- XXX Sanity checker
sparse :: String -> IO ()
sparse str = case (parse mainParser "island" str) of
               Left error -> print error
               Right out  -> print out