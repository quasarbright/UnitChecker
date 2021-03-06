module ParseProgram(program, parseProgram, statement, parseStatement) where
-- TODO error if they try to override an SI unit bc they won't be able to use it

import ParseUtils
import Exprs
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import ParseExpr
import ParseUnit

{--
<program> = <statement>*

<statement> = 'def' <identifier> '=' <expression>
            | 'var' <identifier> '::' <unit>
            | 'derived' <identifier> '=' <unit>
            | 'expr' <expr>
            | 'eq' <expr> '=' <expr>

<expr> = <expr> <binop> <expr>
       | '-' <expr>
       | '(' <expr> ')'
       | <identifier> '(' <args>? ')'
       | <expr> '::' <unit>
       | <number>
       | <identifier>

<args> = <expr> | <expr> ',' <args>

<binop> = [-+*/^]

<unit> = '[' <basePow>* ']'

<basePow> = <identifier> ('^' <integer>)?
--}

program :: Parser (Program SS)
program = Program <$> many statement <?> "program"

parseProgram :: String -> String -> Either ParseError (Program SS)
parseProgram = parse (P.whiteSpace lexer *> program <* eof)

parseStatement :: String -> String -> Either ParseError (Statement SS)
parseStatement = parse (P.whiteSpace lexer *> statement <* eof)

eqTok :: Parser ()
eqTok = P.reservedOp lexer "="

statement :: Parser (Statement SS)
statement =  varDecl
         <|> exprStatement
         <|> eqn
         <|> derivedDecl
         <|> varDef
         <|> funDef
         <?> "statement"

varDecl :: Parser (Statement SS)
varDecl = do
    ((name, u), ss) <- wrapSS $ do
        () <- var
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
        () <- eq
        left <- expr
        () <- eqTok
        right <- expr
        return (left, right)
    return (EqnStatement (Equation left right ss) ss)

derivedDecl :: Parser (Statement SS)
derivedDecl = do
    ((name, u), ss) <- wrapSS $ do
        () <- derived
        name <- ident
        () <- eqTok
        u <- unit
        return (name, u)
    return (DerivedDeclStatement (DerivedDecl name u ss) ss)

varDef :: Parser (Statement SS)
varDef = do
    ((name, e), ss) <- wrapSS $ do
        () <- def
        name <- ident
        () <- eqTok
        e <- expr
        return (name, e)
    return (VarDefStatement name e ss)

funDef :: Parser (Statement SS)
funDef = do
    ((name, sig), ss) <- wrapSS $ do
        fun
        name <- ident
        annotTok
        sig <- signatureP
        return (name, sig)
    return (FunDefStatement name sig ss)
