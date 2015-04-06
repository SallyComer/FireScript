module Main where

import ExecScript
import ScriptBuiltins
import System.Environment (getArgs)
import LineEditor
import System.IO




main :: IO ()
main = do
    (hSetBuffering stdout NoBuffering)
    cmdArgs <- getArgs
    if null cmdArgs
        then putStr "Starting the interpreter!\n" >> startTheThing True
        else do
            val <- runScript (head cmdArgs)
            print val

runScript :: FilePath -> IO ()
runScript fname = do
    text <- readFile fname
    ((importProgram stdEnv text) >>= runMain) >> return ()