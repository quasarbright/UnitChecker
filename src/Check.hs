module Check where
-- TODO make prelude an actual file and link it

import Exprs
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Ratio
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad
import Data.Functor.Identity
import Data.List


-- | custom error type for unit errors
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
    show (IrrationalDimensionlessExponent e a) = "The exponent of a quantity with units must be a ratio of two integers. Exponent "++show e++" is not a ratio of two integers. You might need to simplify. At "++show a
    show (IndivisibleRationalDimensionLessExponent base power pq a) = "Unable to exponentiate unit ["++show base++"^"++show power++"] to the power "++show p++"/"++show q++" since "++show power++"*"++show p++" is not divisible by "++show q++". At "++show a
        where
            p = numerator pq
            q = denominator pq

-- | An environment mapping names to types. Has separate name spaces for variable types, type synonynms, and function types.
--   Also has an incrementing count to ID each association in the environment (hence @Map String (Unit a, Int)@). This is useful for when one program imports another and you only want definitions
--   from the new program.
data TyEnv a = TyEnv {derivedMap :: Map String (Unit a, Int), varMap :: Map String (Unit a, Int), funMap :: Map String (Signature a, Int), count :: Int}

instance Show (TyEnv a) where
    show env =
        [deriveds, vars, funs]
        |> concat
        |> sortOn snd
        |> fmap fst
        |> unlines
        where
            deriveds = mapToVersionedLines " = " (derivedMap env)
            vars = mapToVersionedLines " :: " (varMap env)
            funs = mapToVersionedLines " :: " (funMap env)
            mapToVersionedLines sep m = --showPair sep . swap . unassoc <$> Map.toList m
                Map.toList m
                |> fmap unassoc
                |> fmap (\(ab,n) -> (showPair sep ab, n))
            unassoc (a, (b, c)) = ((a, b), c)
            showPair sep (name, val) = name++sep++show val

emptyEnvironment :: TyEnv a
emptyEnvironment = TyEnv {derivedMap=Map.empty, varMap=Map.empty, funMap=Map.empty, count = 0}

-- | increments the counter for an environment. should be called whenever you add a new environment element
_inc :: TyEnv a -> TyEnv a
_inc env = env{count=count env+1}

-- | add a type synonym to an environment
addDerived :: Ord a => String -> Unit a -> TyEnv a -> TyEnv a
addDerived name unit env = _inc env{derivedMap=Map.insert name (unit, count env) (derivedMap env)}

-- | get the (perhaps partially) expanded version of a type synonym in an environment
lookupDerived :: Ord a => String -> TyEnv a -> Maybe (Unit a)
lookupDerived name env = fst <$> Map.lookup name (derivedMap env)

-- | add a typed variable to an environment
addVar :: Ord a => String -> Unit a -> TyEnv a -> TyEnv a
addVar name unit env = _inc env{varMap=Map.insert name (unit, count env) (varMap env)}

-- | get the type of a variable in an environment
lookupVar :: Ord a => String -> TyEnv a -> Maybe (Unit a)
lookupVar name env = fst <$> Map.lookup name (varMap env)

-- | add a function and its signature to the environment
addFun :: Ord a => String -> Signature a -> TyEnv a -> TyEnv a
addFun name sig env = _inc env{funMap=Map.insert name (sig, count env) (funMap env)}

-- | get the signature of a function in an environment
lookupFun :: Ord a => String -> TyEnv a -> Maybe (Signature a)
lookupFun name env = fst <$> Map.lookup name (funMap env)

-- | get all defined names in the environment, combining all namespaces. Used for autocomplete
getNames :: Ord a => TyEnv a -> [String]
getNames env = nub $ concat [Map.keys (varMap env),Map.keys (derivedMap env),Map.keys (funMap env)]

-- | @envDifference e' e@ returns the definitions in @e'@ that aren't in @e@
--   Uses the IDs on associations to filter out old ones.
--   Assumes @e'@ imports @e@ so its ID's are all greater
envDifference :: Ord a => TyEnv a -> TyEnv a -> TyEnv a
envDifference e' e = e'{ varMap=mapFilter $ varMap e'
                        , derivedMap=mapFilter $ derivedMap e'
                        , funMap=mapFilter $ funMap e'
                        }
    where
        c = count e
        mapFilter m' = Map.filter (\(_,n') -> n' >= c) m'

infixl 2 |>
-- | Function application pipe. @x |> f |> g |> h@ is @h (g (f x))@. Useful for visually chronological composition
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | initial environment for type checking (AKA prelude). Requires a dummy tag for tagging units
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
    |> addFun "cos" (Signature [dimensionless] dimensionless a)
    |> addFun "exp" (Signature [dimensionless] dimensionless a)

-- | Convert a type environment, which maps names to units, to a well-formedness environment, which only cares about names being in scope
tyEnvToWFEnv :: Ord a => TyEnv a -> ([String], [String], [String])
tyEnvToWFEnv env = ( Map.keys $ derivedMap env
                    , Map.keys $ varMap env
                    , Map.keys $ funMap env
                    )

-- | The initial environment (AKA prelude), but only containing information about which names are in scope. Used for well-formedness check
initialWFEnv :: ([String], [String], [String])
initialWFEnv = tyEnvToWFEnv (initialEnv ())

-- | Get the derived unit names that occur in a unit. For example, @getUnitFreeVars (parseUnit "[J N s]") == [J, N]@
--   Used for checking if a derived unit is in scope
getUnitFreeVars :: Unit a -> [(String, a)]
getUnitFreeVars unit = dNames where
    dNames =  Maybe.catMaybes $ getDName <$> Map.keys (bases unit)
    getDName (Derived name a) = Just (name, a)
    getDName _ = Nothing

-- | Check the well-formedness of a unit given which derived units are in scope
checkUnit :: [String] -> Unit a -> [Error a]
checkUnit env unit = ans where
    ans = Maybe.catMaybes $ checkDefined <$> getUnitFreeVars unit
    checkDefined (name, a) = if name `elem` env then Nothing else Just (UnboundDerived name a)

-- | Check the well-formedness of a derived unit declaration given which derived units are in scope
checkDerivedDecl :: [String] -> DerivedDecl a -> [Error a]
checkDerivedDecl env (DerivedDecl _ unit _) = checkUnit env unit

-- | Check the well-formedness of a variable declaration given which derived units are in scope
checkVarDecl :: [String] -> VarDecl a -> [Error a]
checkVarDecl env (VarDecl _ unit _) = checkUnit env unit

-- | Check the well-formedness of an expression given which (derived units, variable names, and function names) are in scope
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
-- | check the well-formedness of a program given which (derived units, variable names, and function names) are in scope
checkWellFormednessWith :: ([String], [String], [String]) -> Program a -> ([Error a], ([String], [String], [String]))
checkWellFormednessWith wfenv (Program statements) = foldl go ([], wfenv) statements where
    go (errs, env@(deriveds, vars, funs)) statement = case statement of
        VarDeclStatement vd@(VarDecl name _ _) _ -> (errs++checkVarDecl deriveds vd, (deriveds, name:vars, funs))
        DerivedDeclStatement dd@(DerivedDecl name _ _) _ -> (errs++checkDerivedDecl deriveds dd, (name:deriveds, vars, funs))
        ExprStatement e _ -> (errs++checkExprWellFormedness env e, env)
        EqnStatement (Equation left right _) _ -> (errs++checkExprWellFormedness env left++checkExprWellFormedness env right, env)
        VarDefStatement name e _ -> (errs++checkExprWellFormedness env e, (deriveds, name:vars, funs))

-- | check the well-formedness of a program using the prelude's defined names as the initial environment
checkWellFormedness :: Program a -> ([Error a], ([String], [String], [String]))
checkWellFormedness = checkWellFormednessWith initialWFEnv

-- | Monad for type checking functions which can raise type errors. Carries a "mutable" type environment as state.
--   Parametrized by tag type @a@ and return type @b@
type EnvProcessor a b = StateT (TyEnv a) (Either (Error a)) b

-- | Monad for type checking functions which cannot raise type errors. Carries a "mutable" type environment as state.
--   Parametrized by tag type @a@ and return type @b@
type InfallibleEnvProcessor a b = State (TyEnv a) b

-- | Type check a program. Assumes program is well-formed
typeCheckProgramEnv :: Ord a => Program a -> EnvProcessor a ()
-- just check all statements, accumulating the environment
typeCheckProgramEnv (Program statements) = sequence_ (checkStatement <$> statements)

-- | Type check a statement, possibly manipulate the environment
checkStatement :: Ord a => Statement a -> EnvProcessor a ()
-- Just try to synthesize the type
checkStatement (ExprStatement e _) = void $ typeSynth e
-- Synthesize both types, make sure they're equal
checkStatement (EqnStatement (Equation left right tag) _) = do 
    leftTy <- typeSynth left
    rightTy <- typeSynth right
    assertSameUnit leftTy rightTy tag
-- add the variable with its type to the environment (well-formedness check guarantees the unit is valid)
checkStatement (VarDeclStatement (VarDecl name unit _) _) = modify $ addVar name unit
-- add the derived unit to the environment
checkStatement (DerivedDeclStatement (DerivedDecl name unit _) _) = modify $ addDerived name unit
-- synthesize the variable's value's type, then add the variable and its type to the environment
checkStatement (VarDefStatement name e _) = do
    unit <- typeSynth e
    modify $ addVar name unit

{--
This is the real meat of the type checker. I use a simple bidirectional type system since there is the polymorphism is simple (just binops) and inference is easy.
Here is the paper this type system is based on:
https://arxiv.org/abs/1908.05839
@misc{dunfield2019bidirectional,
    title={Bidirectional Typing},
    author={Joshua Dunfield and Neel Krishnaswami},
    year={2019},
    eprint={1908.05839},
    archivePrefix={arXiv},
    primaryClass={cs.PL}
}

It's pretty much just type inference though. The only checking rule is

Env |- e => T1    Env |- T1 = T2
--------------------------------
     Env |- e <= T2

for switching from checking mode to synthesis mode


The inference rules are pretty simple and are mostly just an implementation of the laws of units.
Here is an informal description of the rules:
- numbers are inferred to be unitless by default, but they can be annotated to arbitrary units like (9.8 :: [m s^-2]).
- for variables, look them up in the environment and synthesize that type
- two units are the same iff expanding out all derived units to SI units results in identical units
    - it is assumed that units are always in simplest form:
        - [m^a m^b] simplifies to [m^(a+b)] where a and b are integers
        - [m^0] simplifies to [] (unitless)
        - Simplification is implicitly guaranteed since I use a Map from bases to integers
            with exponent addition as a value combiner for common keys
- for addition and subtraction, both operands must have the same units
- for multiplication, operand units are combined (union) and exponents of common base units are added together
- for division, it's the same as multiplication, except exponents are subtracted
- for exponentiation, there are a few rules:
    - unitless^unitless is always ok
    - units^unitless as @a^b@ is only ok iff:
        - @b@ is a ratio of two integers @p/q@ (or easily simplified to one)
        - all units of @a@ have exponents @n@ such that @n*p@ is divisible by @q@.
        The synthesized type is found by transforming all exponents @n@ in the units to @n*p/q@
        Expansion of derived units into SI units may be necessary.
        For example, @(1 :: [J kg])^(1/2)@ looks like it should fail, but expanded, it becomes
        @(1 :: [kg^2 m^2 s^-2])^(1/2)@, which synthesizes the type @[kg m s^-1]@
    - units^units is never ok
    - unitless^units is never ok
- for function calls, the rule is pretty intuitive. The arguments have to match the signature and the return type is synthesized.
    Here it is formally:
        Env(f) = (T1,T2,...,Tn) -> T   Env |- x1 <= T1   Env |- x2 <= T2  ...  Env |- xn <= Tn
        --------------------------------------------------------------------------------------
                                    Env |- (f(x1,x2,...,xn)) => T
    This uses type checking
- for annotations (e :: T), the synthesized type of e must be the same as T
    Formally:
            Env |- e <= T
        --------------------
        Env |- (e :: T) => T
    This uses type checking
--}

-- | Type check an expression, as opposed to type syntheses
typeCheck :: Ord a => Expr a -> Unit a -> EnvProcessor a ()
typeCheck e unit = do
    unit' <- typeSynth e
    assertSameUnit unit unit' (getTag e)

typeSynth :: Ord a => Expr a -> EnvProcessor a (Unit a)
typeSynth e = case e of
    DoubleExpr{} -> return dimensionless
    IntExpr{} -> return dimensionless
    -- simple lookup
    Var name a -> do
        env <- get
        lift $ maybe (Left (UnboundVar name a)) Right (lookupVar name env)
    -- check arguments against signature argument types and synthesize the return type if all is well
    App f args a -> do
        env <- get
        let Signature ins ret _ = Maybe.fromMaybe (error "unbound fun") (lookupFun f env)
        let expected = length ins
        let actual = length args
        -- arity check
        when (expected /= actual) (lift $ Left (ArityError f expected actual a))
        -- check that all the args match the argument types
        zipWithM_ typeCheck args ins
        -- all good, synthesize return type
        return ret
    Prim1 Negate inner _ -> typeSynth inner
    Prim2 prim2 left right a -> do
        leftUnit <- typeSynth left
        rightUnit <- typeSynth right
        -- both operands must be the same, synthesize type
        let plusMinus = do
                    assertSameUnit leftUnit rightUnit a
                    return leftUnit
        case prim2 of
            Plus -> plusMinus
            Minus -> plusMinus
            -- union of bases, adding exponents of common bases
            Times -> return $ multiplyUnits leftUnit rightUnit
            -- union of bases, subtracting exponents of common bases
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
                -- something ^ dimensionfull, mismatch
                | otherwise -> lift $ Left (Mismatch dimensionless rightUnit a)
                where
                    leftBasePows = Map.toList (bases leftUnit)
                    rightBasePows = Map.toList (bases rightUnit)
    Parens inner _ -> typeSynth inner
    -- type check the expression against the annotated type
    Annot inner unit _
        | canBeAnnotated inner -> return unit
        | otherwise -> do
            typeCheck inner unit 
            return unit

-- | Flip an either type.
--   Used when instead of aborting upon an error, you want to keep retrying unless you found an answer,
--   but you still want to know the errors if your retries all fail
flipEither :: Either a b -> Either b a
flipEither (Right b) = Left b
flipEither (Left a) = Right a

-- | Expand a (possibly derived) base unit to SI units given a type environment.
--   Cannot result in an error assuming program well-formedness
expandBaseUnit :: Ord a => BaseUnit a -> InfallibleEnvProcessor a (Unit a)
expandBaseUnit (Derived name _) = do
    env <- get
    expandUnit $ Maybe.fromMaybe (error "unbound derived") (lookupDerived name env)
expandBaseUnit base = return $ fromBasesList [(base, 1)]

-- | Expand a unit's derived units to SI units and simplify the resulting unit
--   Cannot result in an error assuming program well-formedness
expandUnit :: Ord a => Unit a -> InfallibleEnvProcessor a (Unit a)
expandUnit unit = do
        let basePows = Map.toList (bases unit)
        let expandAndRaiseBasePow (base, power) = do
                expandedBase <- expandBaseUnit base
                return $ powUnitInt expandedBase power
        let raisedBases = expandAndRaiseBasePow <$> basePows
        foldr (liftM2 multiplyUnits) (return dimensionless) raisedBases

-- | Assert two units are the same in the current environment and throw a mismatch error if they're not.
--   Uses given tag as the error location.
--   Expands derived units and then compares for equality.
--   Reports error in terms of original, un-expanded units of a mismatch occurs
assertSameUnit :: Ord a => Unit a -> Unit a -> a -> EnvProcessor a ()
assertSameUnit a b tag = do
    -- this nested do weirdness is due to transforming between State and StateT Either monads
    unequal <- mapStateT (return . runIdentity) $ do
        a' <- expandUnit a
        b' <- expandUnit b
        return (a' /= b')
    when unequal (lift $ Left $ Mismatch a b tag)

-- | Check the program with the given dummy tag for generating the initial environment and return the final environment.
--   The dummy tag might be a source span <prelude>:0:0-0:0, for example
checkProgramWith :: Ord a => a -> Program a -> Either [Error a] (TyEnv a)
checkProgramWith dummy = checkProgramWithEnv (initialEnv dummy)

checkProgramWithEnv :: Ord a => TyEnv a -> Program a -> Either [Error a] (TyEnv a)
checkProgramWithEnv env p = do
    let (errs, _) = checkWellFormednessWith (tyEnvToWFEnv env) p
    -- Throw wf errs if there are any
    unless (null errs) (Left errs)
    -- Run the type checker with the initial env passed in and wrap the result in an either
    -- The weirdness is due to needing to put a possible error into a singleton list to make the types line up
    either (Left . return) Right (execStateT (typeCheckProgramEnv p) env)

-- | Run a single statment given an environment and return the resulting environment
--   Used for REPL
runStatement :: Ord a => TyEnv a -> Statement a -> Either [Error a] (TyEnv a)
runStatement env s = checkProgramWithEnv env (Program [s])