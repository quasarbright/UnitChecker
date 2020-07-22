module Parsing(expr, parseExpr) where

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import Data.Functor
import qualified Data.Functor.Identity
import Text.Parsec.Pos
import Exprs
import Control.Monad
import ParseUtils


combineExprSS :: Expr SS -> Expr SS -> SS
combineExprSS e1 e2 = combineSS (getTag e1) (getTag e2)

wrapPrim2 :: Prim2 -> Parser (Expr SS -> Expr SS -> Expr SS)
wrapPrim2 prim2 = P.reservedOp lexer (show prim2) $> op where
    op e1 e2 = Prim2 prim2 e1 e2 (combineExprSS e1 e2)

plus :: Parser (Expr SS -> Expr SS -> Expr SS)
plus = wrapPrim2 Plus

minus :: Parser (Expr SS -> Expr SS -> Expr SS)
minus = wrapPrim2 Minus

times :: Parser (Expr SS -> Expr SS -> Expr SS)
times = wrapPrim2 Times

divide :: Parser (Expr SS -> Expr SS -> Expr SS)
divide = wrapPrim2 Divide

pow :: Parser (Expr SS -> Expr SS -> Expr SS)
pow = wrapPrim2 Pow

-- TODO circular parser loop with a list
type ExprParser = Parser (Expr SS) -> Parser (Expr SS)

exprParsers :: [ExprParser]
exprParsers = [annot, sumDiff, prodQuot, power, uminus, app, atom]



expr :: Parser (Expr SS)
expr = foldr (\ep p -> ep p) (error "you will never arrive at the truth") (cycle exprParsers)

parseExpr :: String -> Either ParseError (Expr SS)
parseExpr source = parse (expr <* eof) "" source

annot :: ExprParser
annot child = do
    ((e, u), ss) <- wrapSS $ do
        e <- child
        () <- annotTok
        u <- unit
        return (e, u)
    return (Annot e u ss)

sumDiff :: ExprParser
sumDiff child = chainl1 child (plus <|> minus) <?> "sum/difference"

prodQuot :: ExprParser
prodQuot child = chainl1 child (times <|> divide) <?> "product/quotient"

power :: ExprParser
power child = chainl1 child pow <?> "exponentiation"

uminus :: ExprParser
uminus child =  try child -- TODO eliminate with (unsigned) natFloat and always uminus for negatives
            <|> (uncurry (Prim1 Negate) <$> wrapSS (minusTok >> child))
            <?> "unary minus"

app :: ExprParser
app child = do
        (name, ss) <- wrapSS ident
        -- parses (e1,e2,...)
        let args = do
            (es, ss') <- wrapSS . inParens $ sepBy1 child commaTok
            return (App name es (combineSS ss ss'))
        args <|> return (Var name ss)
    <|> child
    <?> "function application"

-- TODO combine double and int into one branching parser to avoid this try (maybe use natFloat and rely on uminus)
atom :: ExprParser
atom child =  try double -- try is necessary because -11 would fail double and consume input
    <|> int
    <|> variable
    <|> paren
    <?> "atomic or parenthesized expression"
    where
        paren :: Parser (Expr SS)
        paren = do
            (e, ss) <- wrapSS . inParens $ child
            return (Parens e ss)

double :: Parser (Expr SS)
double = do
    (d, ss) <- wrapSS doubleTok
    return (DoubleExpr d ss)
    <?> "double"

int :: Parser (Expr SS)
int = do
    (n, ss) <- wrapSS intTok
    return (IntExpr n ss)
    <?> "integer"

variable :: Parser (Expr SS)
variable = do
    (name, ss) <- wrapSS ident
    return (Var name ss)
    <?> "variable"

unit = undefined