module Exprs where


import qualified Data.Map as Map
import Data.List
import Control.Monad
import Data.Ratio

type Map k v = Map.Map k v

-- TODO rm radians
-- | SI units + radians + derived
data BaseUnit a = Meter a | Second a | Kilogram a | Ampere a | Kelvin a | Mole a | Candela a | Radian a | Derived String a deriving(Ord)

instance Eq (BaseUnit a) where
    (Derived name1 _) == (Derived name2 _) = name1 == name2
    -- jank, but I don't want to write all those cases and it works
    base1 == base2 = show base1 == show base2

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

-- TODO units need an a
-- | map from base units to their exponents
newtype Unit a = Unit (Map (BaseUnit a) Int) deriving(Eq, Ord)

instance Show (Unit a) where
    show u = "["++unwords (showPair <$> Map.toList (bases u))++"]" where
        showPair (base, power) = show base++"^"++show power

data Signature a = Signature [Unit a]  (Unit a) a deriving(Eq, Ord)

instance Show (Signature a) where
    show (Signature ins ret _) = "("++intercalate ", " (show <$> ins)++") -> "++show ret

-- | a derived unit of energy
joule :: Unit ()
joule = fromBasesList [(Kilogram(), 1), (Meter(), 1), (Second(), -2)]

-- | An example of using derived units in another unit (Joule Seconds)
planckConstantUnits :: Unit ()
planckConstantUnits = fromBasesList [(Derived "J" (), 1), (Second(), 1)]

bases :: Unit a -> Map (BaseUnit a) Int
bases (Unit m) = m

dimensionless :: Ord a => Unit a
dimensionless = fromBasesList []

isDimensionless :: Unit a -> Bool
isDimensionless unit = null $ bases unit

fromBases :: Map (BaseUnit a) Int -> Unit a
fromBases m = Unit m' where
    m' = Map.filter (/= 0) m

fromBasesList :: Ord a => [(BaseUnit a, Int)] -> Unit a
fromBasesList = fromBases . Map.fromListWith (+)

multiplyUnits :: Ord a => Unit a -> Unit a -> Unit a
multiplyUnits a b = fromBases $ Map.unionWith (+) (bases a) (bases b)

divideUnits :: Ord a => Unit a -> Unit a -> Unit a
divideUnits a b = fromBases $ Map.unionWith (-) (bases a) (bases b)

-- | powUnit onFail unit (p % q) multiplies the unit's exponents by p / q
--   if a base unit's power is not divisible by q, call onFail on it
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

data Prim1 = Negate deriving(Eq, Ord)

instance Show Prim1 where
    show Negate = "-"

data Prim2 = Plus | Minus | Times | Divide | Pow deriving(Eq, Ord)

instance Show Prim2 where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Divide = "/"
    show Pow = "^"

-- | A mathematical expression
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

getTag :: Expr a -> a
getTag (IntExpr _ a) = a
getTag (DoubleExpr _ a) = a
getTag (Var _ a) = a
getTag (App _ _ a) = a
getTag (Prim1 _ _ a) = a
getTag (Prim2 _ _ _ a) = a
getTag (Parens _ a) = a
getTag (Annot _ _ a) = a

-- | for something like (9.8 :: m^1 s^-2)
canBeAnnotated :: Expr a -> Bool
canBeAnnotated IntExpr{} = True
canBeAnnotated DoubleExpr{} = True
canBeAnnotated Var{} = False
canBeAnnotated App{} = False
canBeAnnotated (Prim1 Negate e _) = canBeAnnotated e
canBeAnnotated Prim2{} = False
canBeAnnotated (Parens e _) = canBeAnnotated e
canBeAnnotated Annot{} = False


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

-- | A declaration of a derived unit (like joule = kg m / s^2)
data DerivedDecl a = DerivedDecl String (Unit a) a deriving(Eq, Ord, Show)
-- | A mathematical equation
data Equation a = Equation (Expr a) (Expr a) a deriving(Eq, Ord)

instance Show (Equation a) where
    show (Equation left right _) = show left++" = "++show right

-- TODO maybe just get rid of VarDecl and friends
-- Just have VarDecl be a Statement case instead of its own type
data Statement a = ExprStatement (Expr a) a
                 | EqnStatement (Equation a) a
                 | VarDeclStatement (VarDecl a) a
                 | DerivedDeclStatement (DerivedDecl a) a
                 | VarDefStatement String (Expr a) a
                 deriving(Eq, Ord, Show)

-- | A program to be type-checked
newtype Program a = Program [Statement a]