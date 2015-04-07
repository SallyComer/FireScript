module LineEditor where
import ParseScript
import ExecScript
--import System.Console.Haskeline
import Data.List
import ScanScript
import ScriptBuiltins
import ASTData





findCorrectThing (globals, locals, []) = prompt >> return (globals, locals, [])
findCorrectThing (globals, locals, toks) = let
    stmtTrys = map parseStatement (reverse $ inits toks)
    declTrys = map parseDecl (reverse $ inits toks)
    in
        case find eitherToBool stmtTrys of
            Just (Right (a, rest)) -> (exec globals locals a) >>= (\a' -> case a' of
                (Right thing) -> prompt >> return (globals, thing, rest)
                (Left thing) -> putChar '\'' >> putStr (show thing) >> putStr "'\n" >> prompt >> return (globals, locals, rest))
            Nothing -> case find eitherToBool declTrys of
                Just (Right (a, rest)) -> (declare globals a) >>= (\a' -> prompt >> return (a', locals, rest))
                Nothing -> case (head declTrys, head stmtTrys) of
                    (Left declCrap, Left stmtCrap) -> putStrLn ("    fix decl?: " ++ declCrap ++ "\n\n    fix stmt?: " ++ stmtCrap ++ "\n  " ++ show toks) >> scold >> return (globals, locals, toks)




oneStrikeUrOut (globals, locals, []) = soberPrompt >> return (globals, locals)
oneStrikeUrOut (globals, locals, toks) = case parseStatement toks of
    Right (a, rest) -> do
        result <- exec globals locals a
        case result of
            Right newLocals -> do
                soberPrompt
                return (globals, newLocals)
            Left retVal -> do
                putChar '\''
                putStr (show retVal)
                putStr "'\n"
                soberPrompt
                return (globals, locals)
    Left stmtCrap -> case parseDecl toks of
        Right (a, rest) -> do
            newGlobals <- declare globals a
            soberPrompt >> return (newGlobals, locals)
        Left declCrap -> do
            putStr "    Declaration?: "
            putStrLn declCrap
            putStr "\n      Statement?: "
            putStrLn stmtCrap
            putStrLn ("  " ++ show toks)
            soberPrompt
            return (globals, locals)
            


replThing (globals, locals, toks) = do
    text <- getLine
    let toks' = toks ++ (tokenize text)
    findCorrectThing (globals, locals, toks') >>= replThing


replBetter :: (Namespace, Namespace) -> IO ()
replBetter (globals, locals) = do
    text <- getTheText "" Normal
    oneStrikeUrOut (globals, locals, tokenize text) >>= replBetter
    

getTheText :: String -> ReplMode -> IO String
getTheText acc Normal = do
    text <- getLine
    case text of
        ":{" -> blockPrompt >> getTheText "" GettingBlock
        a -> return a
getTheText acc GettingBlock = do
    text <- getLine
    case text of
        ":}" -> return acc
        a -> blockPrompt >> getTheText (acc ++ a) GettingBlock

data ReplMode = Normal | GettingBlock deriving (Show, Eq)

prompt = putStr "Victory!@$ "

scold = putStr "~?~?> "

soberPrompt = putStr "firescript> "

blockPrompt = putStr "firescript| "

eitherToBool (Right _) = True
eitherToBool (Left _) = False




startTheThing False = prompt >> replThing (stdEnv, Namespace [], [])
startTheThing True = soberPrompt >> replBetter (stdEnv, Namespace [])
