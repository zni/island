-- ----------------------------------------------
-- Author  : Matthew Godshall
-- Date    : 2011 May 13 18:16:03
-- 
-- Parser for the interpreter.
-- ----------------------------------------------
module Frontend (lparse) where

import Control.Applicative ((<*))
import Data.Char (isLower)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim (many)

import AST

def = emptyDef{ commentStart = "{-"
              , commentEnd   = "-}"
              , identStart   = letter
              , identLetter = alphaNum
              , opStart      = oneOf "+-*%/=:"
              , reservedOpNames = ["+", "-", "*", "%",
                                   "/", "=", "succ", "pred", ":"]
              , reservedNames = ["if", "then", "else", "let",
                                 "letrec", "in", "val", "proc", 
                                 "type", "fun"]
              }


TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , integer = m_integer
           , symbol = m_symbol
           , whiteSpace = m_whiteSpace 
           , stringLiteral = m_stringLiteral} = makeTokenParser def


-- Parse an identifier with possible type annotation.
varType :: Parser AST
varType = try typed <|> untyped
    where typed = do ident <- m_identifier
                     m_reserved ":"
                     typeN <- typeAnnotParse
                     return $ Var typeN ident
          untyped = do ident <- m_identifier
                       return $ Var TBottom ident

typeAnnotParse :: Parser Type
typeAnnotParse = do
    t <- try arrow <|> unary
    return $ typeStringToType t
    where arrow = do a <- m_identifier `sepBy` m_symbol "->"
                     return a
          unary = do ident <- m_identifier
                     return [ident]


-- XXX Maybe move into AST.hs
typeStringToType :: [String] -> Type
typeStringToType (a:[]) = toType a
typeStringToType (a:xs) = TArrow (toType a) (typeStringToType xs)

toType :: String -> Type
toType str =
    if all isLower str
        then TVar str
        else case str of
                 "String" -> TString
                 "Bool"   -> TBool
                 "Int"    -> TInt
                 "Char"   -> TChar
                 u        -> TUser u


expParser :: Parser AST
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


primExp :: Parser AST
primExp = do
  op <- ops
  exps <- m_parens $ term `sepBy` m_whiteSpace
  if null exps
    then fail "cannot apply a primitive operation to an empty expression list."
    else return $ PrimExp op exps
  

-- Expression terms
term = primExp
    <|> app
    <|> varType
    <|> fmap (\x -> Literal $ LInt x) m_integer
    <|> fmap (\x -> Literal $ LString x) m_stringLiteral
    <?> "term"


-- Parse 'let' expression.
letExp :: Parser AST
letExp = do try $ m_reserved "let"
            b <- many1 ( do { m_reserved "val"
                            ; i <- varType
                            ; m_reservedOp "="
                            ; e <- expParser
                            ; return (i, e)
                            } )
            m_reserved "in"
            e <- expParser
            return (LetExp b e)


-- Parse 'letrec' expression.
letRecExp :: Parser AST
letRecExp = do m_reserved "letrec"
               b <- many1 ( do { procName <- varType
                               ; ids <- m_parens $ varType `sepBy` m_symbol ","
                               ; m_reservedOp "="
                               ; body <- expParser
                               ; return (procName, ids, body)
                               } )
               m_reserved "in"
               e <- expParser
               return (LetRecExp b e)


-- Parse 'type' declaration.
typeDec :: Parser AST
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
ifExp :: Parser AST
ifExp = do m_reserved "if"
           p <- expParser
           m_reserved "then"
           t <- expParser
           m_reserved "else"
           e <- expParser
           return (IfExp p t e)


-- Parse 'proc' expression.
proc :: Parser AST
proc = do m_reserved "proc"
          ids <- m_parens $ varType `sepBy` m_symbol ","
          body <- expParser
          return (ProcExp ids body)


-- Parse 'app' expression.
app :: Parser AST
app = do (op, ops) <- m_parens (do { op <- expParser
                                   ; ops <- many expParser
                                   ; return (op, ops)
                                   } )
         return (AppExp op ops)

topLevel :: Parser AST
topLevel = do
  m_reserved "fun"
  id <- varType
  vars <-  varType `sepBy` m_whiteSpace
  m_reservedOp "="
  expr <- expParser
  return $ TopLevel id vars expr

program :: Parser AST
program = typeDec
       <|> topLevel
       <|> expParser

mainParser :: Parser [AST]
mainParser = m_whiteSpace >> many1 program <* eof


lparse   :: String -> IO [AST]
lparse f = do 
  result <- parseFromFile mainParser f 
  case result of
    Left err   -> error $ show err
    Right expr -> return expr

sparse :: String -> IO ()
sparse str = case (parse mainParser "island" str) of
               Left error -> print error
               Right out  -> print out
