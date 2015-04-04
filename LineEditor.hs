module LineEditor where
import ParseScript
import ExecScript
--import System.Console.Haskeline
import Data.List
import ScanScript
import ScriptBuiltins
import ASTData


findCorrectThing (globals, locals, toks) = let
    stmtTrys = map parseStatement (reverse $ inits toks)
    declTrys = map parseDecl (reverse $ inits toks)
    in
        case find eitherToBool stmtTrys of
            Just (Right (a, rest)) -> (exec globals locals a) >>= (\a' -> case a' of
                (Right thing) -> prompt >> return (globals, thing, rest)
                (Left thing) -> putChar '\'' >> putStr (show thing) >> putStr "'\n" >> prompt >> return (globals, locals, rest))
            Nothing -> case find eitherToBool declTrys of
                Just (Right (a, rest)) -> (declare globals a) >>= (\a' -> return (a', locals, rest))
                Nothing -> case (head declTrys, head stmtTrys) of
                    (Left declCrap, Left stmtCrap) -> putStrLn ("    fix decl?: " ++ declCrap ++ "\n\n    fix stmt?: " ++ stmtCrap ++ "\n  " ++ show toks) >> scold >> return (globals, locals, toks)







replThing (globals, locals, toks) = do
    text <- getLine
    let toks' = toks ++ (tokenize text)
    findCorrectThing (globals, locals, toks') >>= replThing
    


prompt = putStr "Victory!@$ "

scold = putStr "~?~?> "

eitherToBool (Right _) = True
eitherToBool (Left _) = False




startTheThing = replThing (stdEnv, Namespace [], [])