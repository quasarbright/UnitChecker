{-# Language FlexibleContexts #-}
module Main where

import System.Environment
import System.IO
import System.Console.Repline
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict(StateT, evalStateT)
import Control.Monad.State.Class
import Data.List
import Lib
import Check
import ParseUtils(SS, dummySS)
import Exprs

-- TODO argparse for something like check -i fma.unit
-- TODO release packaged binaries on github

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        filePath:_ -> do
            source <- readFile filePath
            runString filePath source

runString :: String -> String -> IO ()
runString filePath source = case parseProgram filePath source of
    Left err -> hPrint stderr err
    Right prog ->
        case checkProgram prog of
            Left errs -> sequence_ (print <$> errs)
            Right env -> print (envDifference env (initialEnv dummySS))

type Repl a = HaskelineT (StateT (TyEnv SS) IO) a

cmd :: String -> Repl ()
cmd input = do
    env <- get
    case parseProgram "<stdin>" input of
        Left err -> liftIO $ hPrint stderr err
        Right prog ->
            case checkProgramWithEnv env prog of
                Left errs -> liftIO $ sequence_ (print <$> errs)
                Right env' -> do
                    put env'
                    liftIO (print (envDifference env' env))

comp :: (Monad m, MonadState (TyEnv SS) m) => WordCompleter m
comp n = do
    env <- get
    let names = concat [ getNames env
                       , ["var", "def", "derived", "fun", "eq", "expr"]
                       , [show (b()) | b <- siUnits]
                       , [":"++name | opt <- opts, name <- synonyms opt]
                       ]
    return $ filter (isPrefixOf n) names

help :: [String] -> Repl ()
help _ = liftIO $ putStrLn (unlines helpLines)

quit :: [String] -> Repl ()
quit _ = abort

listEnv :: [String] -> Repl ()
listEnv _ = do
    env <- get
    liftIO (print env)

data Opt = Opt{ synonyms :: [String]
              , arguments :: [String]
              , runOpt :: [String] -> Repl ()
              , usage :: String  
              }
opts :: [Opt]
opts = 
    [ Opt ["help", "h"] [] help    "display this help information"
    , Opt ["quit", "q"] [] quit    "exit the repl"
    , Opt ["list", "l"] [] listEnv "list all names currently in scope"
    ]

helpLines :: [String]
helpLines = "command -> description":[unwords ((":"++) <$> synonyms opt)++" -> "++usage opt | opt <- opts]

opts' :: [(String, [String] -> Repl ())]
opts' = [(name, runOpt opt) | opt <- opts, name <- synonyms opt]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to UnitChecker!" >> putStrLn "for help, use :h or :help. to quit, use :q or :quit"

repl :: IO ()
repl = flip evalStateT (initialEnv dummySS)
     $ evalRepl (pure "UnitChecker> ") cmd opts' (Just ':') (Word comp) ini