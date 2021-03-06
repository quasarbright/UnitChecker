module Exprs where


import qualified Data.Map as Map
import Data.List
import Control.Monad
import Data.Ratio

type Map k v = Map.Map k v

-- TODO rm radians
-- | Either an SI unit or a derived unit.
--   Equality ignores tags
data BaseUnit a = Meter a | Second a | Kilogram a | Ampere a | Kelvin a | Mole a | Candela a | Radian a | Derived String a

-- | All SI units, but they need a tag
siUnits :: [a -> BaseUnit a]
siUnits = [Meter, Second, Kilogram, Ampere, Kelvin, Mole, Candela, Radian]

-- | Names of all SI units
siUnitNames :: [String]
siUnitNames = [show (b()) | b <- siUnits]

-- equality ignores tags
instance Eq (BaseUnit a) where
    (Derived name1 _) == (Derived name2 _) = name1 == name2
    -- jank, but I don't want to write all those cases and it works
    base1 == base2 = show base1 == show base2

-- comparison ignores tags
instance Ord (BaseUnit a) where
    compare (Derived name1 _) (Derived name2 _) = compare name1 name2
    compare Derived{} _ = GT
    compare _ Derived{} = LT
    -- again jank, but works and saves cases
    compare si1 si2 = compare (show si1) (show si2)

instance Show (BaseUnit a) where
    show Meter{} = "m"
    show Second{} = "s"
    show Kilogram{} = "kg"
    show Ampere{} = "A"
    show Kelvin{} = "K"
    show Mole{} = "mol"
    show Candela{} = "cd"
    show Radian{} = "rad"
    show (Derived name _) = name

-- | map from base units to their exponents
newtype Unit a = Unit {bases :: Map (BaseUnit a) Int} deriving(Eq, Ord)

instance Show (Unit a) where
    show u = "["++unwords (showPair <$> Map.toList (bases u))++"]" where
        showPair (base, 1) = show base
        showPair (base, power) = show base++"^"++show power

-- | Function signature. No currying, so it's just inputs -> output
data Signature a = Signature [Unit a]  (Unit a) a deriving(Eq, Ord)

instance Show (Signature a) where
    show (Signature ins ret _) = "("++intercalate ", " (show <$> ins)++") -> "++show ret

-- | the units for a dimensionless quantity (no units)
dimensionless :: Ord a => Unit a
dimensionless = fromBasesList []

-- | Is the given unit dimensionless?
isDimensionless :: Unit a -> Bool
isDimensionless unit = null $ bases unit

-- | Create a unit from a map from bases to powers
--   Use this instead of the constructor because it filters out [m^0]
fromBases :: Map (BaseUnit a) Int -> Unit a
fromBases m = Unit m' where
    m' = Map.filter (/= 0) m

-- | Create a unit from a list of tuples from bases to powers
fromBasesList :: Ord a => [(BaseUnit a, Int)] -> Unit a
fromBasesList = fromBases . Map.fromListWith (+)

-- | multiply two units together (union of bases, adding exponents of common bases)
multiplyUnits :: Ord a => Unit a -> Unit a -> Unit a
multiplyUnits a b = fromBases $ Map.unionWith (+) (bases a) (bases b)

-- | divide two units (union of bases, subtracting exponents of common bases)
divideUnits :: Ord a => Unit a -> Unit a -> Unit a
divideUnits a b = fromBases $ Map.unionWith (-) (bases a) (bases b)

-- | powUnit unit (p % q) multiplies the unit's exponents by p / q.
--   if a base unit's power n is not divisible by q, return the base unit and n instead (Left)
powUnit :: Ord a => Unit a -> Ratio Int -> Either (BaseUnit a, Int) (Unit a)
powUnit unit pq = Unit . Map.fromList <$> mapM mulPower (Map.toList (bases unit)) where
    mulPower (base, power) = do
        let p = numerator pq
        let q = denominator pq
        when (q == 0) (Left (base, power))
        when (mod (power * p) q /= 0) (Left (base, power))
        return (base, power * p `div` q)

-- | Raise a unit to an integer p power by multiplying its exponents by p
powUnitInt :: Ord a => Unit a -> Int -> Unit a
powUnitInt unit p = Unit . Map.fromList $ mulPower <$> Map.toList (bases unit) where
    mulPower (base, power) =
        (base, power * p)

-- | unary operation
data Prim1 = Negate deriving(Eq, Ord)

instance Show Prim1 where
    show Negate = "-"

-- | binary operation
data Prim2 = Plus | Minus | Times | Divide | Pow deriving(Eq, Ord)

instance Show Prim2 where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Divide = "/"
    show Pow = "^"

-- | A mathematical expression, possibly annotated with units
data Expr a = IntExpr Int a
          | DoubleExpr Double a
          | Var String a
          | App String [Expr a] a
          | Prim2 Prim2 (Expr a) (Expr a) a
          | Prim1 Prim1 (Expr a) a
          | Parens (Expr a) a
          | Annot (Expr a) (Unit a) a
          deriving(Eq, Ord)

instance Show (Expr a) where
    show (IntExpr n _) = show n
    show (DoubleExpr d _) = show d
    show (Var name _) = name
    show (App f args _) = f++"("++intercalate ", " (show <$> args)++")"
    show (Prim2 prim2 left right _) = unwords [show left, show prim2, show right]
    show (Prim1 prim1 e _) = show prim1++show e
    show (Parens e _) = "("++show e++")"
    show (Annot e u _) = show e++" :: "++show u

-- | get the tag of an expression
getTag :: Expr a -> a
getTag (IntExpr _ a) = a
getTag (DoubleExpr _ a) = a
getTag (Var _ a) = a
getTag (App _ _ a) = a
getTag (Prim1 _ _ a) = a
getTag (Prim2 _ _ _ a) = a
getTag (Parens _ a) = a
getTag (Annot _ _ a) = a

-- | True for constant, unitless expressions. For example, (9.8 :: m^1 s^-2) is ok
canBeAnnotated :: Expr a -> Bool
canBeAnnotated IntExpr{} = True
canBeAnnotated DoubleExpr{} = True
canBeAnnotated Var{} = False
canBeAnnotated App{} = False
canBeAnnotated (Prim1 Negate e _) = canBeAnnotated e
canBeAnnotated Prim2{} = False
canBeAnnotated (Parens e _) = canBeAnnotated e
canBeAnnotated Annot{} = False

-- | Try to convert an expression to a ratio.
--   Reduces on */+-, but only reduces on ^ if power is an integer
--   Does not try for doubles or variables.
asRatio :: Expr a -> Maybe (Ratio Int)
asRatio (IntExpr n _) = return (n % 1)
asRatio DoubleExpr{} = Nothing
asRatio Var{} = Nothing
asRatio (Prim2 prim2 left right _) = do
    leftRatio <- asRatio left
    rightRatio <- asRatio right
    case prim2 of
        Plus -> return (leftRatio + rightRatio)
        Minus -> return (leftRatio - rightRatio)
        Times -> return (leftRatio * rightRatio)
        Divide -> return (leftRatio / rightRatio)
        Pow
            | mod (numerator rightRatio) (denominator rightRatio) == 0 -> return (leftRatio ^ (numerator rightRatio `div` denominator rightRatio))
            | otherwise -> Nothing
asRatio (Prim1 Negate e _) = do
    eRatio <- asRatio e
    return (negate eRatio)
asRatio App{} = Nothing
asRatio (Parens e _) = asRatio e
asRatio (Annot e _ _) = asRatio e

-- | A declaration of a variable's unit
data VarDecl a = VarDecl String (Unit a) a deriving(Eq, Ord, Show)

-- | A declaration of a derived unit (like J = [kg m / s^2])
data DerivedDecl a = DerivedDecl String (Unit a) a deriving(Eq, Ord, Show)

-- | A mathematical equation
data Equation a = Equation (Expr a) (Expr a) a deriving(Eq, Ord)

instance Show (Equation a) where
    show (Equation left right _) = show left++" = "++show right

-- TODO maybe just get rid of VarDecl and friends
-- TODO add monomorphic function declaration
-- Just have VarDecl be a Statement case instead of its own type
data Statement a = ExprStatement (Expr a) a
                 | EqnStatement (Equation a) a
                 | VarDeclStatement (VarDecl a) a
                 | DerivedDeclStatement (DerivedDecl a) a
                 | VarDefStatement String (Expr a) a
                 | FunDefStatement String (Signature a) a
                 deriving(Eq, Ord, Show)

-- | A program to be type-checked
newtype Program a = Program [Statement a]