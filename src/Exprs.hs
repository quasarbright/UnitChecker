module Exprs where


import qualified Data.Map as Map
import Data.List

type Error = String

type Map k v = Map.Map k v

-- | SI units + radians
data BaseUnit = Meter | Second | Kilogram | Ampere | Kelvin | Mole | Candela | Radian deriving(Eq, Ord)

instance Show BaseUnit where
    show Meter = "m"
    show Second = "s"
    show Kilogram = "kg"
    show Ampere = "A"
    show Kelvin = "K"
    show Mole = "mol"
    show Candela = "cd"
    show Radian = "rad"

-- | map from base units to their exponents
newtype Unit = Unit (Map BaseUnit Int) deriving(Eq, Ord)

instance Show Unit where
    show u = unwords $ showPair <$> Map.toList (bases u) where
        showPair (base, power) = show base++"^"++show power

-- | a derived unit of energy
joule :: Unit
joule = fromBasesList [(Kilogram, 1), (Meter, 1), (Second, -2)]

bases :: Unit -> Map BaseUnit Int
bases (Unit m) = m

fromBases :: Map BaseUnit Int -> Unit
fromBases m = Unit m' where
    m' = Map.filter (/= 0) m

fromBasesList :: [(BaseUnit, Int)] -> Unit
fromBasesList = fromBases . Map.fromList

multiplyUnits :: Unit -> Unit -> Unit
multiplyUnits a b = fromBases $ Map.unionWith (+) (bases a) (bases b)

divideUnits :: Unit -> Unit -> Unit
divideUnits a b = fromBases $ Map.unionWith (-) (bases a) (bases b)

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
data Expr = IntExpr Int
          | DoubleExpr Double
          | Var String
          | App String [Expr]
          | Prim2 Prim2 Expr Expr
          | Prim1 Prim1 Expr
          | Parens Expr
          | Annot Expr Unit
          deriving(Eq, Ord)

instance Show Expr where
    show (IntExpr n) = show n
    show (DoubleExpr d) = show d
    show (Var name) = name
    show (App f args) = f++"("++intercalate ", " (show <$> args)++")"
    show (Prim2 prim2 left right) = unwords [show left, show prim2, show right]
    show (Prim1 prim1 e) = show prim1++show e
    show (Parens e) = "("++show e++")"
    show (Annot e u) = show e++" :: "++show u

-- | A declaration of a variable's unit
data VarDecl = VarDecl String Unit deriving(Eq, Ord)

-- | A declaration of a derived unit (like joule = kg m / s^2)
data DerivedDecl = DerivedDecl String Unit

-- | A mathematical equation
data Equation = Equation Expr Expr

instance Show Equation where
    show (Equation left right) = show left++" = "++show right

-- | A program to be type-checked
data Program = Program
    { derivedDecls :: [DerivedDecl]
    , varDecls :: [VarDecl]
    , equations :: [Equation]
    , exprs :: [Expr]
    }

emptyProgram :: Program
emptyProgram = Program{derivedDecls=[], varDecls=[], equations=[], exprs=[]}

addDerivedDecl :: DerivedDecl -> Program -> Program
addDerivedDecl dd prog = prog{derivedDecls=derivedDecls prog ++ [dd]}

addVarDecl :: VarDecl -> Program -> Program
addVarDecl vd prog = prog{varDecls=varDecls prog ++ [vd]}

addEquation :: Equation -> Program -> Program
addEquation eq prog = prog{equations=equations prog ++ [eq]}

addExpr :: Expr -> Program -> Program
addExpr e prog = prog{exprs=exprs prog ++ [e]}