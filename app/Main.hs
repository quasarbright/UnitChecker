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

-- | Either runs a file or runs the REPL
main :: IO ()
main = do
    args <- getArgs
    case args of
        -- no file, run the repl
        [] -> repl
        -- run the file
        filePath:_ -> do
            source <- readFile filePath
            runString filePath source

-- | Run the given file name and string, printing output
runString :: String -> String -> IO ()
runString filePath source = case parseProgram filePath source of
    -- report parse error
    Left err -> hPrint stderr err
    -- run the program
    Right prog ->
        case checkProgram prog of
            -- report well-formedness/type checking errors
            Left errs -> sequence_ (print <$> errs)
            -- success! show definitions in their file (don't include the prelude)
            Right env -> print (envDifference env (initialEnv dummySS))

-- | Stateful REPL for running statements and viewing definitions
type Repl a = HaskelineT (StateT (TyEnv SS) IO) a

-- | run the input as a statement and print any new definitions. The environment is maintained throughout the REPL
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

-- | (Stateful) Autocomplete based on keywords and names in scope
comp :: (Monad m, MonadState (TyEnv SS) m) => WordCompleter m
comp n = do
    env <- get
    let names = concat [ getNames env
                       , ["var", "def", "derived", "fun", "eq", "expr"]
                       , siUnitNames
                       , [":"++name | opt <- opts, name <- synonyms opt]
                       ]
    return $ filter (isPrefixOf n) names

-- | :help displays help text
help :: [String] -> Repl ()
help _ = liftIO $ putStrLn (unlines helpLines)

-- | :quit exits the repl
quit :: [String] -> Repl ()
quit _ = abort

-- | :list shows all names and their units
listEnv :: [String] -> Repl ()
listEnv _ = do
    env <- get
    liftIO (print env)

-- | Data for a @:help@-like command
data Opt = Opt{ synonyms :: [String]
              , arguments :: [String]
              , runOpt :: [String] -> Repl ()
              , usage :: String  
              }

-- | @:help@-like commands
opts :: [Opt]
opts = 
    [ Opt ["help", "h"] [] help    "display this help information"
    , Opt ["quit", "q"] [] quit    "exit the repl"
    , Opt ["list", "l"] [] listEnv "list all names currently in scope"
    ]

-- | Help text lines describing available @:help@-like commands
helpLines :: [String]
helpLines = "command -> description":[unwords ((":"++) <$> synonyms opt)++" -> "++usage opt | opt <- opts]

-- | @:help@-like commands for defining the repl
opts' :: [(String, [String] -> Repl ())]
opts' = [(name, runOpt opt) | opt <- opts, name <- synonyms opt]

-- | Welcome message
ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to UnitChecker!" >> putStrLn "for help, use :h or :help. to quit, use :q or :quit"

-- | Actual repl definition. Evaluating this runs the repl until the user exits
repl :: IO ()
repl = flip evalStateT (initialEnv dummySS)
     $ evalRepl (pure "UnitChecker> ") cmd opts' (Just ':') (Word comp) ini