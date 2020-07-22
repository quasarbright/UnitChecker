module Lib(checkProgram, parseProgram) where

import Exprs
import Check
import ParseProgram
import ParseUtils
import qualified Data.Map as Map

-- | type checks a program and returns the final type environment
checkProgram :: Program SS -> Either [Error SS] (TyEnv SS)
checkProgram p = rmInitialsEnv <$> checkProgramWith dummySS p

rmInitials :: (Ord a, Eq b) => Map a b -> Map a b -> Map a b
rmInitials m mi = Map.differenceWithKey go m mi
    where
        go k v _
            | Map.lookup k m == Map.lookup k mi = Nothing
            | otherwise = Just v

rmInitialsEnv :: TyEnv SS -> TyEnv SS
rmInitialsEnv e = e{ derivedMap=rmInitials (derivedMap e) (derivedMap ei)
                   , varMap=rmInitials (varMap e) (varMap ei)
                   , funMap=rmInitials (funMap e) (funMap ei)
                   }
    where ei = initialEnv dummySS