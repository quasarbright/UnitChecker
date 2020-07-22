module parseUtils where

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import Data.Functor
import qualified Data.Functor.Identity
import Text.Parsec.Pos
import Exprs
import Control.Monad

data SourceSpan = SourceSpan {beg::SourcePos, end::SourcePos}
type SS = SourceSpan
instance Show SourceSpan where
    show ss = name++" "++show startLine++":"++show startCol++"-"++show endLine++show endCol where
        name = sourceName (beg ss)
        startLine = sourceLine (beg ss)
        startCol = sourceColumn (beg ss)
        endLine = sourceLine (end ss)
        endCol = sourceColumn (end ss)
-- dummySS :: SourceSpan
-- dummySS = SourceSpan (newPos "" 0 0) (newPos "" 0 0)

lang :: P.GenLanguageDef String () Data.Functor.Identity.Identity
lang = P.LanguageDef{
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = True,
    P.identStart = letter <|> char '_' :: Parser Char,
    P.identLetter = letter <|> digit <|> char '_',
    P.opStart = oneOf ":!#$%&*+.,/<=>?@\\^|-~",
    P.opLetter = oneOf ":!#$%&*+.,/<=>?@\\^|-~",
    P.reservedNames = ["var", "def", "fun", "expr", "eq", "derived"],
    P.reservedOpNames = ["+", "-", "*", "/", "^", "=", "::", ","],
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

inParens :: Parser a -> Parser a
inParens = P.parens lexer

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

annotTok :: Parser ()
annotTok = P.reservedOp lexer "::"

minusTok :: Parser ()
minusTok = P.reservedOp lexer "-"

commaTok :: Parser ()
commaTok = P.reservedOp lexer ","

ident :: Parser String
ident = P.identifier lexer

combineSS :: SS -> SS -> SS
combineSS ss1 ss2 = SourceSpan (beg ss1) (end ss2)