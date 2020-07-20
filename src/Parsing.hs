module Parsing where

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import Data.Functor
import qualified Data.Functor.Identity
import Text.Parsec.Pos
import Exprs

data SourceSpan = SourceSpan {beg::SourcePos, end::SourcePos}
type SS = SourceSpan
instance Show SourceSpan where
    show ss = name++" "++show startLine++":"++show startCol++"-"++show endLine++show endCol where
        name = sourceName (beg ss)
        startLine = sourceLine (beg ss)
        startCol = sourceColumn (beg ss)
        endLine = sourceLine (end ss)
        endCol = sourceColumn (end ss)
dummySS :: SourceSpan
dummySS = SourceSpan (newPos "" 0 0) (newPos "" 0 0)

lang :: P.GenLanguageDef String () Data.Functor.Identity.Identity
lang = P.LanguageDef{
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = True,
    P.identStart = letter <|> char '_' :: Parser Char,
    P.identLetter = letter <|> digit <|> char '_',
    P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.reservedNames = ["var", "def", "fun", "expr", "eq", "derived"],
    P.reservedOpNames = ["+", "-", "*", "/", "^", "="],
    P.caseSensitive = True}

lexer :: P.GenTokenParser String () Data.Functor.Identity.Identity
lexer = P.makeTokenParser lang

-- decorates a parser with the source span over the entirety of the parse
wrapSS :: Parser a -> Parser (a, SS)
wrapSS p = do
    beg <- getPosition
    a <- p
    end <- getPosition
    return (a, SourceSpan beg end)

intTok :: Parser Int
intTok = char '-' $> negate <*> go
      <|> go
      where
          go = fromIntegral <$> P.integer lexer

doubleTok :: Parser Double
doubleTok = try (char '-' $> negate <*> float)
         <|> try float
         <|> (fromIntegral <$> intTok)
         where
             float = P.float lexer

keyWordTok :: String -> Parser ()
keyWordTok name = P.reserved lexer name

var :: Parser ()
var = keyWordTok "var"

def :: Parser ()
def = keyWordTok "def"

fun :: Parser ()
fun = keyWordTok "fun"

derived :: Parser ()
derived = keyWordTok "derived"

eq :: Parser ()
eq = keyWordTok "eq"

exprKWD :: Parser ()
exprKWD = keyWordTok "expr"

ident :: Parser String
ident = P.identifier lexer

combineSS :: SS -> SS -> SS
combineSS ss1 ss2 = SourceSpan (beg ss1) (end ss2)

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


annot :: ExprParser
annot = _

sumDiff :: ExprParser
sumDiff = _

prodQuot :: ExprParser
prodQuot = _

power :: ExprParser
power = _

uminus :: ExprParser
uminus = _

app :: ExprParser
app = _

atom :: ExprParser
atom = _


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
