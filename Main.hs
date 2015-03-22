module Main where

import ExecScript
import ScriptBuiltins





runScript :: FilePath -> IO Value
runScript fname = do
    text <- readFile fname
    runMain $ (importProgram stdEnv text)