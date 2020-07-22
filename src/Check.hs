module Check where

import Exprs
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Ratio
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad
import Data.Functor.Identity
import Data.List

data Error a = UnboundDerived String a
             | UnboundVar String a
             | UnboundFun String a
             | Mismatch (Unit a) (Unit a) a
             | ArityError String Int Int a
             | IrrationalDimensionlessExponent (Expr a) a
             | IndivisibleRationalDimensionLessExponent (BaseUnit a) Int (Ratio Int) a
             deriving(Eq, Ord)

instance Show a => Show (Error a) where
    show (UnboundDerived name a) = "Unbound derived unit \""++name++"\" at "++show a
    show (UnboundVar name a) = "Unbound variable \""++name++"\" at "++show a
    show (UnboundFun name a) = "Unbound function \""++name++"\" at "++show a
    show (Mismatch expected actual a) 
        | isDimensionless expected = "Unit mismatch: expected a dimensionless quantity, but got units of "++show actual++" at "++show a
        | isDimensionless actual = "Unit mismatch: expected units of "++show expected++", but got a dimensionless quantity at "++show a
        | otherwise = "Unit mismatch: Expected units of "++show expected++", but got units of "++show actual++" at "++show a
    show (ArityError name expected actual a) = "Arity error: function "++show name++" expects "++show expected++" arguments, but "++show actual++" arguments were given at "++show a
    show (IrrationalDimensionlessExponent e a) = "The exponent of a quantity with units must be obviously rational. Exponent "++show e++" is not obviously rational. Try simplifying. At "++show a
    show (IndivisibleRationalDimensionLessExponent base power pq a) = "Unable to exponentiate unit "++show base++"^"++show power++" to the power "++show p++"/"++show q++" since "++show base++"*"++show p++" is not divisible by "++show q++". At "++show a
        where
            p = numerator pq
            q = denominator pq

data TyEnv a = TyEnv {derivedMap :: Map String (Unit a), varMap :: Map String (Unit a), funMap :: Map String (Signature a)}

instance Show (TyEnv a) where
    show env = intercalate "\n" (deriveds ++ vars ++ funs)
        where
            deriveds = showMap " = " (derivedMap env)
            vars = showMap " :: " (varMap env)
            funs = showMap " :: " (funMap env)
            showMap sep m = showPair sep <$> Map.toList m
            showPair sep (name, val) = name++sep++show val

emptyEnvironment :: TyEnv a
emptyEnvironment = TyEnv {derivedMap=Map.empty, varMap=Map.empty, funMap=Map.empty}

addDerived :: Ord a => String -> Unit a -> TyEnv a -> TyEnv a
addDerived name unit env = env{derivedMap=Map.insert name unit (derivedMap env)}

addVar :: Ord a => String -> Unit a -> TyEnv a -> TyEnv a
addVar name unit env = env{varMap=Map.insert name unit (varMap env)}

addFun :: Ord a => String -> Signature a -> TyEnv a -> TyEnv a
addFun name sig env = env{funMap=Map.insert name sig (funMap env)}

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

initialEnv :: Ord a => a -> TyEnv a
initialEnv a =
    emptyEnvironment
    |> addDerived "N" (fromBasesList [(Kilogram a, 1), (Meter a, 1), (Second a, -2)])
    |> addDerived "J" (fromBasesList [(Derived "N"  a, 1), (Meter a, 1)])
    |> addDerived "c" (fromBasesList [(Ampere a, 1), (Second a, 1)])
    |> addDerived "hz" (fromBasesList [(Second a, -1)])
    |> addVar "g" (fromBasesList [(Meter a, 1), (Second a, -2)])
    |> addVar "c" (fromBasesList [(Meter a, 1), (Second a, -1)])
    |> addVar "pi" dimensionless
    |> addVar "e" dimensionless
    |> addFun "sin" (Signature [dimensionless] dimensionless a)
    |> addFun "exp" (Signature [dimensionless] dimensionless a)

initialWFEnv :: ([String], [String], [String])
initialWFEnv = ( Map.keys $ derivedMap (initialEnv ())
               , Map.keys $ varMap (initialEnv ())
               , Map.keys $ funMap (initialEnv ())
               )

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
--                                        deriveds, vars,     funs
checkWellFormedness :: Program a -> ([Error a], ([String], [String], [String]))
checkWellFormedness (Program statements) = foldl go ([], initialWFEnv) statements where
    go (errs, env@(deriveds, vars, funs)) statement = case statement of
        VarDeclStatement vd@(VarDecl name _ _) _ -> (errs++checkVarDecl deriveds vd, (deriveds, name:vars, funs))
        DerivedDeclStatement dd@(DerivedDecl name _ _) _ -> (errs++checkDerivedDecl deriveds dd, (name:deriveds, vars, funs))
        ExprStatement e _ -> (errs++checkExprWellFormedness env e, env)
        EqnStatement (Equation left right _) _ -> (errs++checkExprWellFormedness env left++checkExprWellFormedness env right, env)
        VarDefStatement name e _ -> (errs++checkExprWellFormedness env e, (deriveds, name:vars, funs))

type EnvProcessor a b = StateT (TyEnv a) (Either (Error a)) b
type InfallibleEnvProcessor a b = State (TyEnv a) b

-- | assumes program is well-formed
typeCheckProgramEnv :: Ord a => Program a -> EnvProcessor a ()
typeCheckProgramEnv (Program statements) = sequence_ (checkStatement <$> statements)

checkStatement :: Ord a => Statement a -> EnvProcessor a ()
checkStatement (ExprStatement e _) = void $ typeSynth e
checkStatement (EqnStatement (Equation left right _) _) = sequence_ $ typeSynth <$> [left, right]
checkStatement (VarDeclStatement (VarDecl name unit _) _) = modify $ addVar name unit
checkStatement (DerivedDeclStatement (DerivedDecl name unit _) _) = modify $ addDerived name unit
checkStatement (VarDefStatement name e _) = do
    unit <- typeSynth e
    modify $ addVar name unit

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
                    -- if any succeed, return it. Otherwise, keep trying
                    let flipped = do
                            (base, power) <- flipEither $ powUnit leftUnit rightRatio
                            let leftUnitExpanded = evalState (expandUnit leftUnit) env
                            _ <- flipEither $ powUnit leftUnitExpanded rightRatio
                            -- want to report the error in terms of the derived units
                            return (IndivisibleRationalDimensionLessExponent base power rightRatio a)
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

-- | expands derived units to SI units and aggregates to one unit
expandUnit :: Ord a => Unit a -> InfallibleEnvProcessor a (Unit a)
expandUnit unit = do
        let basePows = Map.toList (bases unit)
        let expandAndRaiseBasePow (base, power) = do
                expandedBase <- expandBaseUnit base
                return $ powUnitInt expandedBase power
        let raisedBases = expandAndRaiseBasePow <$> basePows
        foldr (liftM2 multiplyUnits) (return dimensionless) raisedBases

assertSameUnit :: Ord a => Unit a -> Unit a -> a -> EnvProcessor a ()
assertSameUnit a b tag = do
    -- this weirdness is due to transforming between Identity and Either StateT's
    unequal <- mapStateT (return . runIdentity) $ do
        a' <- expandUnit a
        b' <- expandUnit b
        return (a' /= b')
    when unequal (lift $ Left $ Mismatch a b tag)

-- | check the program with the given dummy tag for items of the initial environment and return the final environment.
--   This might be a source span <prelude>:0:0-0:0, for example
checkProgramWith :: Ord a => a -> Program a -> Either [Error a] (TyEnv a)
checkProgramWith dummy p = do
    let (errs, _) = checkWellFormedness p
    unless (null errs) (Left errs)
    either (Left . return) Right (execStateT (typeCheckProgramEnv p) (initialEnv dummy))