import Test.HUnit
import Exprs
import Check
import Control.Monad.Trans.State.Lazy
import Data.Ratio

teq :: (Eq a, Show a) => String -> a -> a -> Test
teq name a b = TestCase (assertEqual name a b)

tpass :: Test
tpass = TestCase $ assertEqual "pass" True True

tExpandBase :: String -> BaseUnit () -> Unit () -> Test
tExpandBase name base unit = teq name unit (evalState (expandBaseUnit base) (initialEnv()))

-- unexpanded then expanded unit
tExpandUnit :: String -> Unit() -> Unit() -> Test
tExpandUnit name unit expanded = teq name expanded (evalState (expandUnit unit) (initialEnv()))

m :: BaseUnit()
m = Meter()
kg :: BaseUnit()
kg = Kilogram()
s :: BaseUnit()
s = Second()
a :: BaseUnit()
a = Ampere()
der :: String -> BaseUnit()
der name = Derived name ()

expandBaseTests :: Test
expandBaseTests = TestLabel "expand base" $ TestList [
        tExpandBase "m" m (fromBasesList [(m, 1)]),
        tExpandBase "s" s (fromBasesList [(s, 1)]),
        tExpandBase "N" (der "N") (fromBasesList [(kg, 1), (m, 1), (s, -2)]),
        tExpandBase "J" (der "J") (fromBasesList [(kg, 1), (m, 2), (s, -2)]),
        tExpandBase "c" (der "c") (fromBasesList [(a, 1), (s, 1)]),
        tpass
    ]

expandUnitTests :: Test
expandUnitTests = TestLabel "expand units" $ TestList [
        -- a few of these are really tests of fromBasesList
        tExpandUnit "dimensionless" dimensionless dimensionless,
        tExpandUnit "m^0" (fromBasesList [(m, 0)]) dimensionless,
        tExpandUnit "m^1" (fromBasesList [(m, 1)]) (fromBasesList [(m, 1)]),
        tExpandUnit "m^2" (fromBasesList [(m, 2)]) (fromBasesList [(m, 2)]),
        tExpandUnit "m*m" (fromBasesList [(m, 1), (m, 1)]) (fromBasesList [(m, 2)]),
        tExpandUnit "J" (fromBasesList [(der "J", 1)]) (fromBasesList [(kg, 1), (m, 2), (s, -2)]),
        tExpandUnit "J*m" (fromBasesList [(der "J", 1), (m, 1)]) (fromBasesList [(kg, 1), (m, 3), (s, -2)]),
        tExpandUnit "J*J" (fromBasesList [(der "J", 1), (der "J", 1)]) (fromBasesList [(kg, 2), (m, 4), (s, -4)]),
        tExpandUnit "J^2" (fromBasesList [(der "J", 2)]) (fromBasesList [(kg, 2), (m, 4), (s, -4)]),
        tExpandUnit "J^2*J" (fromBasesList [(der "J", 2), (der "J", 1)]) (fromBasesList [(kg, 3), (m, 6), (s, -6)]),
        tExpandUnit "N" (fromBasesList [(der "N", 1)]) (fromBasesList [(kg, 1), (m, 1), (s, -2)]),
        tExpandUnit "N*m" (fromBasesList [(der "N", 1), (m, 1)]) (fromBasesList [(kg, 1), (m, 2), (s, -2)]),
        tExpandUnit "N*N" (fromBasesList [(der "N", 1), (der "N", 1)]) (fromBasesList [(kg, 2), (m, 2), (s, -4)]),
        tExpandUnit "N*J" (fromBasesList [(der "N", 1), (der "J", 1)]) (fromBasesList [(kg, 2), (m, 3), (s, -4)]),
        tExpandUnit "N^2*J" (fromBasesList [(der "N", 2), (der "J", 1)]) (fromBasesList [(kg, 3), (m, 4), (s, -6)]),
        tExpandUnit "N^2*J*m" (fromBasesList [(der "N", 2), (der "J", 1), (m, 1)]) (fromBasesList [(kg, 3), (m, 5), (s, -6)]),
        tpass
    ]

tSynth :: Expr () -> Unit () -> Test
tSynth e unit = teq ("synthesize "++show e) (return unit) (evalStateT (typeSynth e) (initialEnv()))

tSynthError :: Expr () -> Error () -> Test
tSynthError e err = teq ("synthesize "++show e) (Left err) (evalStateT (typeSynth e) (initialEnv()))

int n = IntExpr n ()
double d = DoubleExpr d ()
var name = Var name ()
annot e unit = Annot e unit ()
app f args = App f args ()
prim1 p e = Prim1 p e ()
neg = prim1 Negate
prim2 p a b = Prim2 p a b ()
plus = prim2 Plus
minus = prim2 Minus
times = prim2 Times
divide = prim2 Divide
pow = prim2 Pow

gUnit = fromBasesList [(m, 1), (s, -2)]
newtons = fromBasesList [(kg, 1), (m, 1), (s, -2)]

synthTests :: Test
synthTests = TestLabel "type synthesis" $ TestList [
        tSynth (int 1) dimensionless,
        tSynth (double 1.0) dimensionless,
        tSynth (int 1 `plus` int 1) dimensionless,
        tSynth (var "g") (fromBasesList [(m, 1), (s, -2)]),
        tSynth (var "g" `pow` int 2) (fromBasesList [(m, 2), (s, -4)]),
        tSynth (var "g" `pow` (int 2 `divide` int 1)) (fromBasesList [(m, 2), (s, -4)]),
        tSynth ((var "g" `pow` int 2) `pow` (int 1 `divide` int 2)) (fromBasesList [(m, 1), (s, -2)]),
        tSynth (var "g" `plus` var "g") (fromBasesList [(m, 1), (s, -2)]),
        tSynth (var "g" `minus` var "g") (fromBasesList [(m, 1), (s, -2)]),
        tSynth (var "g" `times` var "g") (fromBasesList [(m, 2), (s, -4)]),
        tSynth (var "g" `divide` var "g") dimensionless,
        tSynth (var "g" `times` (var "g" `pow` int 2)) (fromBasesList [(m, 3), (s, -6)]),
        tSynth ((var "g" `times` (var "g" `pow` int 2)) `pow` (int 1 `divide` int 3)) (fromBasesList [(m, 1), (s, -2)]),
        tSynth (var "g" `times` int 2) (fromBasesList [(m, 1), (s, -2)]),
        tSynth (annot (int 2) dimensionless) dimensionless,
        tSynth (annot (int 2) (fromBasesList [(der "N", 1)])) (fromBasesList [(der "N", 1)]),
        tSynth (annot (neg $ int 2) (fromBasesList [(der "N", 1)])) (fromBasesList [(der "N", 1)]),
        tSynth (annot (neg $ neg $ int 2) (fromBasesList [(der "N", 1)])) (fromBasesList [(der "N", 1)]),
        tSynth (double 1.0 `pow` double 2.5) dimensionless,
        tSynth (app "sin" [var "pi"]) dimensionless,
        tSynth (((int 2 `annot` fromBasesList [(kg, 1)]) `times` var "g") `annot` fromBasesList [(der "N", 1)]) (fromBasesList [(der "N", 1)]),
        -- now with errors
        tSynthError (var "g" `plus` var "pi") (Mismatch (fromBasesList [(m, 1), (s, -2)]) dimensionless ()),
        tSynthError (var "g" `minus` var "pi") (Mismatch (fromBasesList [(m, 1), (s, -2)]) dimensionless ()),
        tSynthError (var "g" `plus` var "c") (Mismatch (fromBasesList [(m, 1), (s, -2)]) (fromBasesList [(m, 1), (s, -1)]) ()),
        tSynthError (var "g" `minus` var "c") (Mismatch (fromBasesList [(m, 1), (s, -2)]) (fromBasesList [(m, 1), (s, -1)]) ()),
        tSynthError (var "g" `pow` var "c") (Mismatch dimensionless (fromBasesList [(m, 1), (s, -1)]) ()),
        tSynthError (int 1 `pow` var "c") (Mismatch dimensionless (fromBasesList [(m, 1), (s, -1)]) ()),
        tSynthError (var "g" `pow` (int 3 `divide` int 2)) (IndivisibleRationalDimensionLessExponent m 1 (3 % 2) ()),
        tSynthError ((var "g" `plus` var "g") `pow` (int 3 `divide` int 2)) (IndivisibleRationalDimensionLessExponent m 1 (3 % 2) ()),
        tSynthError (var "g" `pow` double 2) (IrrationalDimensionlessExponent (double 2) ()),
        tSynthError (var "g" `pow` var "pi") (IrrationalDimensionlessExponent (var "pi") ()),
        tSynthError (var "g" `annot` fromBasesList [(der "N", 1)]) (Mismatch (fromBasesList [(der "N", 1)]) (fromBasesList [(m, 1), (s, -2)]) ()),
        tSynthError ((var "g" `annot` fromBasesList [(der "N", 1)]) `plus` var "pi") (Mismatch (fromBasesList [(der "N", 1)]) (fromBasesList [(m, 1), (s, -2)]) ()),
        tSynthError ("sin" `app` [var "pi", var "pi"]) (ArityError "sin" 1 2 ()),
        tSynthError ("sin" `app` [var "g", var "c"]) (ArityError "sin" 1 2 ()),
        tSynthError ("sin" `app` []) (ArityError "sin" 1 0 ()),
        tSynthError ("sin" `app` [var "g"]) (Mismatch dimensionless gUnit ()),
        tpass
    ]

tests :: Test
tests = TestList [
        expandBaseTests,
        expandUnitTests,
        synthTests,
        tpass
    ]

main :: IO Counts
main = runTestTT tests
