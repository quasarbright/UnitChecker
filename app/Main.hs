module Main where

import System.Environment
import System.IO
import System.Console.Repline
import Control.Monad.IO.Class
import Data.List
import Lib

-- TODO repl
-- TODO release packaged binaries on github

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl -- TODO document change to piping
        filePath:_ -> do
            source <- readFile filePath
            runString filePath source

runString :: String -> String -> IO ()
runString filePath source = case parseProgram filePath source of
    Left err -> hPrint stderr err
    Right prog ->
        case checkProgram prog of
            Left errs -> sequence_ (print <$> errs)
            Right env -> print env

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ runString "<stdin>" input

completer :: Monad m => WordCompleter m
completer n = do
  let names = ["eq", "expr", "var", "def", "derived", "fun", ":help", ":list"]
  return $ filter (isPrefixOf n) names


help :: [String] -> Repl ()
help _ = liftIO $ print "help" -- TODO help

quit :: [String] -> Repl ()
quit _ = abort

-- listEnv :: [String] -> Repl ()
-- listEnv _ = liftIO $ print env

otherCommands :: [(String, [String] -> Repl ())]
otherCommands = 
    [ ("help", help)  -- :help
    , ("q", quit)
    , ("quit", quit)
    ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to UnitChecker!" >> putStrLn "for help, use :help. to quit, use :q or :quit"

repl :: IO ()
repl = evalRepl (pure ">>> ") cmd otherCommands (Just ':') (Word completer) ini