module ParseProgram(program, parseProgram) where

import ParseUtils
import Exprs
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import ParseExpr
import ParseUnit

program :: Parser (Program SS)
program = Program <$> many statement <?> "program"

parseProgram :: String -> String -> Either ParseError (Program SS)
parseProgram = parse (P.whiteSpace lexer *> program <* eof)

eqTok :: Parser ()
eqTok = P.reservedOp lexer "="

semiTok :: Parser ()
semiTok = P.reservedOp lexer ";"

statement :: Parser (Statement SS)
statement =  varDecl <* semiTok
         <|> try exprStatement <* semiTok -- TODO eliminate try by changing expr keyword to something that doesn't start with an e
         <|> eqn <* semiTok
         <|> try derivedDecl <* semiTok -- TODO eliminate try by changing derived/def keyword. use let for def
         <|> varDef <* semiTok
         <?> "statement"

varDecl :: Parser (Statement SS)
varDecl = do
    ((name, u), ss) <- wrapSS $ do
        () <- keyWordTok "var"
        name <- ident
        () <- annotTok
        u <- unit
        return (name, u)
    return (VarDeclStatement  (VarDecl name u ss) ss)

exprStatement :: Parser (Statement SS)
exprStatement = uncurry ExprStatement <$> wrapSS (keyWordTok "expr" *> expr)

eqn :: Parser (Statement SS)
eqn = do
    ((left, right), ss) <- wrapSS $ do
        () <- keyWordTok "eq"
        left <- expr
        () <- eqTok
        right <- expr
        return (left, right)
    return (EqnStatement (Equation left right ss) ss)

derivedDecl :: Parser (Statement SS)
derivedDecl = do
    ((name, u), ss) <- wrapSS $ do
        () <- keyWordTok "derived"
        name <- ident
        () <- eqTok
        u <- unit
        return (name, u)
    return (DerivedDeclStatement (DerivedDecl name u ss) ss)

varDef :: Parser (Statement SS)
varDef = do
    ((name, e), ss) <- wrapSS $ do
        () <- keyWordTok "def"
        name <- ident
        () <- eqTok
        e <- expr
        return (name, e)
    return (VarDefStatement name e ss)