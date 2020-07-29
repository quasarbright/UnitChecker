module ParseUnit(unit, parseUnit, signatureP, parseSignature) where

import ParseUtils
import Exprs
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import qualified Data.Maybe as Maybe

{--
<unit> = '[' <basePow>* ']'

<basePow> = <identifier> ('^' <integer>)?
--}

baseOfString :: String -> SS -> BaseUnit SS
baseOfString s = Maybe.fromMaybe (Derived s) (lookup s siMap)
    where
        siMap :: [(String, SS -> BaseUnit SS)]
        siMap = [(show (b dummySS), b) | b <- siUnits]
base :: Parser (BaseUnit SS)
base = uncurry baseOfString <$> wrapSS ident <?> "base unit"

powTok :: Parser ()
powTok = P.reservedOp lexer "^"

-- m   or    m^2
-- optional ^n
basePow :: Parser (BaseUnit SS, Int)
basePow = do
    b <- base
    let power = do
            () <- powTok
            p <- intTok
            return (b, p)
    power <|> return (b, 1) -- want [kg m s^-1] to be ok. might be problematic, but it should just fail without consuming otherwise

-- [kg m s^-1]
unit :: Parser (Unit SS)
unit = fromBasesList <$> (P.brackets lexer (many basePow) <?> "unit")


parseUnit :: String -> Either ParseError (Unit SS)
parseUnit = parse (P.whiteSpace lexer *> unit <* eof) ""

signatureP :: Parser (Signature SS)
signatureP = do
    ((args, ret), ss) <- wrapSS $ do
        args <- inParens $ sepBy unit commaTok
        arrowTok
        ret <- unit
        return (args, ret)
    return $ Signature args ret ss

parseSignature :: String -> Either ParseError (Signature SS)
parseSignature = parse (P.whiteSpace lexer *> signatureP <* eof) ""