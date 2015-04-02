module Main where

import ExecScript
import ScriptBuiltins
import System.Environment (getArgs)




main :: IO ()
main = do
    cmdArgs <- getArgs
    if null cmdArgs
        then putStrLn "You've really got to give me a file to work with!"
        else do
            val <- runScript (head cmdArgs)
            print val

runScript :: FilePath -> IO ()
runScript fname = do
    text <- readFile fname
    ((importProgram stdEnv text) >>= runMain) >> return ()