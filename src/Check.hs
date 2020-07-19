module Check(checkWellFormedness, typeCheckProgram) where

import Exprs
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import Data.Ratio
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad

data Error a = UnboundDerived String a
             | UnboundVar String a
             | UnboundFun String a
             | Mismatch (Unit a) (Unit a) a
             | ArityError String Int Int a
             | IrrationalDimensionlessExponent (Expr a) a
             | IndivisibleRationalDimensionLessExponent (BaseUnit a) (Ratio Int) a

data TyEnv a = TyEnv {derivedMap :: Map String (Unit a), varMap :: Map String (Unit a), funMap :: Map String (Signature a)}


addDerived :: Ord a => String -> Unit a -> TyEnv a -> TyEnv a
addDerived name unit env = env{derivedMap=Map.insert name unit (derivedMap env)}

addVar :: Ord a => String -> Unit a -> TyEnv a -> TyEnv a
addVar name unit env = env{varMap=Map.insert name unit (varMap env)}

addFun :: Ord a => String -> Signature a -> TyEnv a -> TyEnv a
addFun name sig env = env{funMap=Map.insert name sig (funMap env)}

getUnitFreeVars :: Unit a -> [(String, a)]
getUnitFreeVars unit = dNames where
    dNames =  Maybe.catMaybes $ getDName <$> Map.keys (bases unit)
    getDName (Derived name a) = Just (name, a)
    getDName _ = Nothing

checkUnit :: [String] -> Unit a -> [Error a]
checkUnit env unit = ans where
    ans = Maybe.catMaybes $ checkDefined <$> getUnitFreeVars unit
    checkDefined (name, a) = if name `elem` env then Nothing else Just (UnboundDerived name a)

-- | names in scope, decls to check
checkDerivedDecl :: [String] -> DerivedDecl a -> [Error a]
checkDerivedDecl env (DerivedDecl _ unit _) = checkUnit env unit

checkVarDecl :: [String] -> VarDecl a -> [Error a]
checkVarDecl env (VarDecl _ unit _) = checkUnit env unit

checkExprWellFormedness :: ([String], [String], [String]) -> Expr a -> [Error a]
checkExprWellFormedness env@(deriveds, vars, funs) e =
    case e of
        Var name a -> [UnboundVar name a | name `notElem` vars]
        DoubleExpr{} -> []
        IntExpr{} -> []
        Prim1 _ e' _ -> recurse e'
        Prim2 _ left right _ -> recurse left++recurse right
        App f args a -> [UnboundFun f a | f `notElem` funs]++concatMap recurse args
        Parens e' _ -> recurse e'
        Annot e' unit _ -> recurse e'++checkUnit deriveds unit
    where
        recurse = checkExprWellFormedness env

checkWellFormedness :: Program a -> ([Error a], ([String], [String], [String]))
checkWellFormedness (Program statements) = foldr go ([], ([], [], [])) statements where
    go statement (errs, env@(deriveds, vars, funs)) = case statement of
        VarDeclStatement vd@(VarDecl name _ _) -> (errs++checkVarDecl deriveds vd, (deriveds, name:vars, funs))
        DerivedDeclStatement dd@(DerivedDecl name _ _) -> (errs++checkDerivedDecl deriveds dd, (name:deriveds, vars, funs))
        ExprStatement e -> (errs++checkExprWellFormedness env e, env)
        EqnStatement (Equation left right _) -> (errs++checkExprWellFormedness env left++checkExprWellFormedness env right, env)

type EnvProcessor a b = StateT (TyEnv a) (Either (Error a)) b

-- | assumes program is well-formed
typeCheckProgram :: Program a -> Either (Error a) ()
typeCheckProgram (Program statements) = return ()

typeCheck :: Ord a => TyEnv a -> Expr a -> Unit a -> Either (Error a) ()
typeCheck env e unit = do
    unit' <- typeSynth env e
    assertSameUnit env unit unit' (getTag e)

typeSynth :: Ord a => TyEnv a -> Expr a -> Either (Error a) (Unit a)
typeSynth env e = case e of
    DoubleExpr{} -> return dimensionless
    IntExpr{} -> return dimensionless
    Var name _ -> return $ Maybe.fromMaybe (error "unbound var") (Map.lookup name (varMap env))
    App f args a -> do
        let Signature ins ret _ = Maybe.fromMaybe (error "unbound fun") (Map.lookup f (funMap env))
        let expected = length ins
        let actual = length args
        -- arity check
        when (expected /= actual) (Left (ArityError f expected actual a))
        -- check that all the args match the argument types
        zipWithM_ (typeCheck env) args ins
        -- all good
        return ret
    Prim1 Negate inner _ -> typeSynth env inner
    Prim2 prim2 left right a -> do
        leftUnit <- typeSynth env left
        rightUnit <- typeSynth env right
        let plusMinus = do
                    assertSameUnit env leftUnit rightUnit a
                    return leftUnit
        case prim2 of
            Plus -> plusMinus
            Minus -> plusMinus
            Times -> return $ multiplyUnits leftUnit rightUnit
            Divide -> return $ divideUnits leftUnit rightUnit
            Pow
                -- both dimensionless
                | and $ null <$> [leftBasePows, rightBasePows] -> return leftUnit
                -- dimensionful ^ dimensionless, must be rational p/q and pows must be divisible by q
                | null rightBasePows -> do
                    let maybeRightRatio = asRatio right
                    rightRatio <- Maybe.maybe (Left (IrrationalDimensionlessExponent right a)) Right maybeRightRatio
                    let onFail base pq = IndivisibleRationalDimensionLessExponent base pq a
                    -- if any succeed, return it. Otherwise, keep trying
                    let flipped = do
                        err1 <- flipEither $ powUnit onFail leftUnit rightRatio
                        let leftUnitExpanded = expandUnit env leftUnit
                        _ <- flipEither $ powUnit onFail leftUnitExpanded rightRatio
                        return err1
                    flipEither flipped
                -- dimensionless ^ dimensionfull, mismatch
                | otherwise -> Left (Mismatch dimensionless rightUnit a)
                where
                    leftBasePows = Map.toList (bases leftUnit)
                    rightBasePows = Map.toList (bases rightUnit)
    Parens inner _ -> typeSynth env inner
    Annot inner unit _
        | canBeAnnotated inner -> return unit
        | otherwise -> do
            typeCheck env inner unit 
            return unit

flipEither :: Either a b -> Either b a
flipEither (Right b) = Left b
flipEither (Left a) = Right a

expandBaseUnit :: Ord a => TyEnv a -> BaseUnit a -> Unit a
expandBaseUnit env (Derived name _) = expandUnit env $ Maybe.fromMaybe (error "unbound derived") (Map.lookup name (derivedMap env))
expandBaseUnit _ base = fromBasesList [(base, 1)]

-- TODO test
-- | expands derived units to SI units and aggregates to one unit
expandUnit :: Ord a => TyEnv a -> Unit a -> Unit a
expandUnit env (Unit m) = ans where
    basePows = Map.toList m
    go (base, pow) = (expandBaseUnit env base, pow)
    unitPows = go <$> basePows
    units = [powUnitTruncate unit (power % 1) | (unit, power) <- unitPows]
    ans = foldr multiplyUnits dimensionless units

assertSameUnit :: Ord a => TyEnv a -> Unit a -> Unit a -> a -> Either (Error a) ()
assertSameUnit env a b tag = when (expandUnit env a /= expandUnit env b) (Left $ Mismatch a b tag)

