module Lib
    ( someFunc
    ) where

import Exprs
import Check
import ParseExpr
import ParseUnit

someFunc :: IO ()
someFunc = putStrLn "someFunc"