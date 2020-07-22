module Main where

import System.Environment
import System.IO
import Lib


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            source <- getContents
            go "<stdin>" source
        filePath:_ -> do
            source <- readFile filePath
            go filePath source

go :: String -> String -> IO ()
go filePath source = case parseProgram filePath source of
    Left err -> hPrint stderr err
    Right prog ->
        case checkProgram prog of
            Left errs -> sequence_ (print <$> errs)
            Right env -> print env