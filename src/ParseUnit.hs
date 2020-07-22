module ParseUnit(unit, parseUnit) where

import ParseUtils
import Exprs
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as P
import Control.Applicative hiding (Const)
import Data.Functor
import qualified Data.Maybe as Maybe

baseOfString :: String -> SS -> BaseUnit SS
baseOfString s = Maybe.fromMaybe (Derived s) (lookup s siMap)
    where
        siMap :: [(String, SS -> BaseUnit SS)]
        siMap = [ ("m", Meter)
                , ("s", Second)
                , ("kg", Kilogram)
                , ("A", Ampere)
                , ("K", Kelvin)
                , ("mol", Mole)
                , ("cd", Candela)
                , ("rad", Radian)
                ]

-- m
base :: Parser (BaseUnit SS)
base = uncurry baseOfString <$> wrapSS ident

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
unit = fromBasesList <$> P.brackets lexer (many basePow)

parseUnit :: String -> Either ParseError (Unit SS)
parseUnit = parse (P.whiteSpace lexer *> unit <* eof) ""