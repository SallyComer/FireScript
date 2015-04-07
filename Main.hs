module Main where

import ExecScript
import ScriptBuiltins
import System.Environment (getArgs)
import LineEditor
import System.IO
import Control.Monad (foldM)




main :: IO ()
main = do
    (hSetBuffering stdout NoBuffering)
    cmdArgs <- getArgs
    if null cmdArgs
        then putStr "Starting the interpreter!\n" >> startTheThing True
        else do
            let (flags, fnames) = splitArgs cmdArgs
            env <- foldM obtainProgram stdEnv fnames
            decideAction flags env
            

runScript :: FilePath -> IO ()
runScript fname = do
    text <- readFile fname
    ((importProgram stdEnv text) >>= runMain) >> return ()


loadForRepl :: Namespace -> IO ()
loadForRepl globals = soberPrompt >> replBetter (globals, Namespace [])


decideAction :: [String] -> Namespace -> IO ()
decideAction args globals | "-i" `elem` args = loadForRepl globals
    | otherwise = runMain globals >> return ()



splitArgs :: [String] -> ([String], [String])
splitArgs args = (flags, fnames) where
    isFlag (x:xs) = x == '-'
    flags = filter isFlag args
    fnames = filter (not . isFlag) args