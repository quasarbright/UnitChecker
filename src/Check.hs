module Check(checkWellFormedness, typeCheckProgram) where

import Exprs
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import Data.Ratio
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad
import Data.Functor.Identity

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
        VarDeclStatement vd@(VarDecl name _ _) _ -> (errs++checkVarDecl deriveds vd, (deriveds, name:vars, funs))
        DerivedDeclStatement dd@(DerivedDecl name _ _) _ -> (errs++checkDerivedDecl deriveds dd, (name:deriveds, vars, funs))
        ExprStatement e _ -> (errs++checkExprWellFormedness env e, env)
        EqnStatement (Equation left right _) _ -> (errs++checkExprWellFormedness env left++checkExprWellFormedness env right, env)

type EnvProcessor a b = StateT (TyEnv a) (Either (Error a)) b
type InfallibleEnvProcessor a b = State (TyEnv a) b

-- | assumes program is well-formed
typeCheckProgram :: Ord a => Program a -> EnvProcessor a ()
typeCheckProgram (Program statements) = sequence_ (checkStatement <$> statements)

checkStatement :: Ord a => Statement a -> EnvProcessor a ()
checkStatement (ExprStatement e _) = void $ typeSynth e
checkStatement (EqnStatement (Equation left right _) _) = sequence_ $ typeSynth <$> [left, right]
checkStatement (VarDeclStatement (VarDecl name unit _) _) = modify $ addVar name unit
checkStatement (DerivedDeclStatement (DerivedDecl name unit _) _) = modify $ addDerived name unit

typeCheck :: Ord a => Expr a -> Unit a -> EnvProcessor a ()
typeCheck e unit = do
    unit' <- typeSynth e
    assertSameUnit unit unit' (getTag e)

typeSynth :: Ord a => Expr a -> EnvProcessor a (Unit a)
typeSynth e = case e of
    DoubleExpr{} -> return dimensionless
    IntExpr{} -> return dimensionless
    Var name a -> do
        env <- get
        lift $ maybe (Left (UnboundVar name a)) Right (Map.lookup name (varMap env))
    App f args a -> do
        env <- get
        let Signature ins ret _ = Maybe.fromMaybe (error "unbound fun") (Map.lookup f (funMap env))
        let expected = length ins
        let actual = length args
        -- arity check
        when (expected /= actual) (lift $ Left (ArityError f expected actual a))
        -- check that all the args match the argument types
        zipWithM_ typeCheck args ins
        -- all good
        return ret
    Prim1 Negate inner _ -> typeSynth inner
    Prim2 prim2 left right a -> do
        leftUnit <- typeSynth left
        rightUnit <- typeSynth right
        let plusMinus = do
                    assertSameUnit leftUnit rightUnit a
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
                    env <- get
                    let maybeRightRatio = asRatio right
                    rightRatio <- lift $ Maybe.maybe (Left (IrrationalDimensionlessExponent right a)) Right maybeRightRatio
                    let onFail base pq = IndivisibleRationalDimensionLessExponent base pq a
                    -- if any succeed, return it. Otherwise, keep trying
                    let flipped = do
                        err <- flipEither $ powUnit onFail leftUnit rightRatio
                        -- TODO make it so expansion doesn't return Either bc well-formedness should guarantee it
                        let leftUnitExpanded = evalState (expandUnit leftUnit) env
                        _ <- flipEither $ powUnit onFail leftUnitExpanded rightRatio
                        return err
                    lift $ flipEither flipped
                -- dimensionless ^ dimensionfull, mismatch
                | otherwise -> lift $ Left (Mismatch dimensionless rightUnit a)
                where
                    leftBasePows = Map.toList (bases leftUnit)
                    rightBasePows = Map.toList (bases rightUnit)
    Parens inner _ -> typeSynth inner
    Annot inner unit _
        | canBeAnnotated inner -> return unit
        | otherwise -> do
            typeCheck inner unit 
            return unit

flipEither :: Either a b -> Either b a
flipEither (Right b) = Left b
flipEither (Left a) = Right a

expandBaseUnit :: Ord a => BaseUnit a -> InfallibleEnvProcessor a (Unit a)
expandBaseUnit (Derived name _) = do
    env <- get
    expandUnit $ Maybe.fromMaybe (error "unbound derived") (Map.lookup name (derivedMap env))
expandBaseUnit base = return $ fromBasesList [(base, 1)]

-- TODO test
-- | expands derived units to SI units and aggregates to one unit
expandUnit :: Ord a => Unit a -> InfallibleEnvProcessor a (Unit a)
expandUnit unit = do
        let basePows = Map.toList (bases unit)
        let expandAndRaiseBasePow (base, power) = do
            expandedBase <- expandBaseUnit base
            return $ powUnitTruncate expandedBase (power % 1)
        let raisedBases = expandAndRaiseBasePow <$> basePows
        foldr (liftM2 multiplyUnits) (return dimensionless) raisedBases

-- TODO test if you can compare StateT's for equality. seems weird
assertSameUnit :: Ord a => Unit a -> Unit a -> a -> EnvProcessor a ()
assertSameUnit a b tag = do
    -- this weirdness is due to transforming between Identity and Either StateT's
    unequal <- mapStateT (return . runIdentity) $ do
        a' <- expandUnit a
        b' <- expandUnit b
        return (a' /= b')
    when unequal (lift $ Left $ Mismatch a b tag)
