# UnitChecker

A tool for checking whether physics equations and expressions break the rules of units. i.e. you can't add meters and kilograms or raise e to the power of 2 meters

For an example displaying the syntax and all the features, see `examples/fma.unit`

## Features
* supports basic arithmetic expressions and (monomorphic) functions
* checks that an expression is valid in terms of units
* checks whether two expressions have the same units in an equation
* define your own derived units
* declare a variable's units and use it in expressions
* define a variable to infer its units, then use it in expressions
* common derived units (i.e. `N = [kg m s^-2]`), variables (i.e. `g :: [m s^-2]`), and functions (i.e. `sin :: ([]) -> []`) are included in prelude

## Examples
More detailed examples can be found in the `examples` directory. `examples/fma.unit` is a particularly good example of all features and syntax

Here is an example of a program:

```
var m :: [kg]
var v :: [m s^-1]
def p = m*v
eq (1/2) * m * v^2 = p^2 / (2*m)
def KE = (1/2) * m * v^2
```
This program succeeds and outputs the following:
```
KE :: [kg m^2 s^-2]
m :: [kg]
p :: [kg m s^-1]
v :: [m s^-1]
```

When a program runs, it either outputs an error or outputs all variables with their units and all derived units with their definitions.

Running this
```
var m :: [kg]

var a :: [m s^ -2]

def F = m * a 

// should give a mismatch error
expr F + m
```
outputs the error
```
Unit mismatch: Expected units of [kg m s^-2], but got units of [kg] at examples/mismatch.unit:8:6-8:11
```

## Syntax
a UnitChecker program consists of a list of statements.
A statement can declare the units of a variable, define a variable and infer its units,
define a derived unit (like Newtons), check the units of an expression,
and check the units of both sides of an equation and check that they're equivalent

An expression is an arithmetic expression of variables and numbers with operators `+-*/^`, function calls, and unit annotations

A unit is written like so: `[kg m^2 s^-2]`

Here is the context free grammar for a program:
```bnf
<program> = <statement>*

<statement> = 'def' <identifier> '=' <expression>
            | 'var' <identifier> '::' <unit>
            | 'derived' <identifier> '=' <unit>
            | 'expr' <expr>
            | 'eq' <expr> '=' <expr>

<expr> = <expr> <binop> <expr>
       | '-' <expr>
       | '(' <expr> ')'
       | <identifier> '(' <args>? ')'
       | <expr> '::' <unit>
       | <number>
       | <identifier>

<args> = <expr> | <expr> ',' <args>

<binop> = [-+*/^]

<unit> = '[' <basePow>* ']'

<basePow> = <identifier> ('^' <integer>)?
```

## Running
You can run a file like this:
```
unitChecker path/to/file.unit
```
You can also pipe from stdin:
```
cat path/to/file.unit | unitChecker
```
if you are running from source, use `stack run` or `stack exec UnitChecker-exe`:
```
stack run path/to/file.unit
```
## dependencies
* parsec
* containers