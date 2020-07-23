module Lib(checkProgram, checkProgramWithEnv, runStatement, parseProgram, parseStatement) where

import Exprs
import Check
import ParseProgram
import ParseUtils

-- | type checks a program and returns the final type environment
checkProgram :: Program SS -> Either [Error SS] (TyEnv SS)
checkProgram p = checkProgramWith dummySS p