module Lib(checkProgram, parseProgram) where

import Exprs
import Check
import ParseProgram
import ParseUtils

-- | type checks a program and returns the final type environment
checkProgram :: Program SS -> Either [Error SS] (TyEnv SS)
checkProgram = checkProgramWith dummySS