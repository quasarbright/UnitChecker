module StateTPractice where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad

{--
type Error = String

type Env = [(String, Int)]

data E = Var String | Val Int | Add E E | Let String E E

type EnvProcessor a = State Env a

lookupE :: String -> EnvProcessor (Maybe Int)
lookupE name = lookup name <$> get
    -- do
    --     env <- get
    --     return (lookup name env)


eval :: E -> EnvProcessor (Either Error Int)
eval (Val n) = return (Right n)
eval (Var x) = do
    maybeVal <- lookupE x
    return $ maybe (Left "unbound") Right maybeVal
eval (Add left right) = do
    leftEither <- eval left
    rightEither <- eval right
    return $ liftM2 (+) leftEither rightEither
eval (Let x valExpr body) = do
    valEither <- eval valExpr
    case valEither of
        Left{} -> return valEither
        Right val -> do
            modify ((x, val):)
            eval body

initialEnv :: Env
initialEnv = []

evalExpr :: E -> Either Error Int
evalExpr e = evalState  (eval e) initialEnv

e1 :: E
e1 = Let "x" (Val 1) (Let "y" (Val 2) (Add (Var "x") (Var "y")))

e2 :: E
e2 = Add (Add (Var "x") (Val 1)) (Val 2)

--}

-- OK now with a transformer

type Error = String

type Env = [(String, Int)]

data E = Var String | Val Int | Add E E | Let String E E

type EnvProcessor a = StateT Env (Either Error) a

lookupE :: String -> EnvProcessor Int
lookupE name = do
    env <- get
    -- OK so lift is like a half-return
    -- EnvProcessor is (StateT Env (Either Error) a) so return is a -> StateT Env (Either Error) a = a,
    -- but lift is Either Error a -> StateT Env (Either Error)
    lift (maybe (Left ("unbound "++name)) Right (lookup name env))

eval :: E -> EnvProcessor Int
eval (Val n) = return n
eval (Var x) = lookupE x
eval (Add left right) = liftM2 (+) (eval left) (eval right)
eval (Let x valExpr body) = do
    val <- eval valExpr
    modify ((x, val):)
    eval body

initialEnv :: Env
initialEnv = []

evalExpr :: E -> Either Error Int
evalExpr e = evalStateT (eval e) initialEnv

e1 :: E
e1 = Let "x" (Val 1) (Let "y" (Val 2) (Add (Var "x") (Var "y")))

e2 :: E
e2 = Add (Add (Var "x") (Val 1)) (Val 2)