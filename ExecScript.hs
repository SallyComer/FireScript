module ExecScript where
import ParseScript


data Value = NumberV Int
    | StringV String
    | ListV [Value]
    | FuncV (Namespace -> [Value] -> Value)
    | Void

instance Eq Value where
    (StringV a) == (StringV b) = a == b
    (ListV a) == (ListV b) = a == b
    (NumberV a) == (NumberV b) = a == b
    Void == Void = True
    _ == _ = False

data Namespace = Namespace [(String, Value)]

data WithArgs = WithArgs Namespace Namespace

class Searchable a where
    search :: String -> a -> Either String Value

instance Searchable Namespace where
    search str (Namespace stuff) = case lookup str stuff of
        Just val -> Right val
        Nothing -> Left str

instance Searchable WithArgs where
    search str (WithArgs a b) = case search str a of
        Right val -> Right val
        _ -> search str b

fallbackSearch str a b = search str (WithArgs a b)

varEq :: Searchable a => String -> Value -> a -> Bool
varEq str val a = case search str a of
    Right val' -> val == val'
    Left _ -> False

updateNames :: Namespace -> Namespace -> Namespace
updateNames os@(Namespace old) ns@(Namespace new) = Namespace (newvars ++ old) where
    overlap = filter (nameExists os) (map fst new)
    newvals = fmap (\a -> unsafeSearch a ns) overlap
    newvars = zip overlap newvals

nameExists :: Namespace -> String -> Bool
nameExists scope str = case search str scope of
    Right _ -> True
    Left _ -> False

unsafeSearch str scope = case search str scope of
    Right a -> a

unEither (Right a) = a
unEither (Left a) = error (show a)

evaluate :: Namespace -> Namespace -> Expr -> Value
evaluate globals locals (Number i) = NumberV i
evaluate globals locals (Str s) = StringV s
evaluate globals locals (Parens thing) = evaluate globals locals thing
evaluate globals locals (Operator op thing1 thing2) = callOperator globals op (evaluate globals locals thing1) (evaluate globals locals thing2)
evaluate globals locals (Call func stuff) = callFunction globals (evaluate globals locals (Name func)) (map (evaluate globals locals) stuff)
evaluate globals locals (List stuff) = ListV $ map (evaluate globals locals) stuff
evaluate globals locals (Name blah) = unEither $ fallbackSearch blah locals globals

createFunction :: Declaration -> [Value] -> Value
createFunction (FuncDec _ args body) = undefined


callOperator = undefined

callFunction :: Namespace -> Value -> [Value] -> Value
callFunction globals (FuncV func) args = func globals args


exec :: Namespace -> Namespace -> Statement -> Either Value Namespace
exec globals ls@(Namespace locals) (Assign str val) = Right $ Namespace $ (str, evaluate globals ls val):locals
exec globals ls@(Namespace locals) (Do val) = Right $ Namespace $ ("_", evaluate globals ls val):locals
exec globals ls@(Namespace locals) (Return thing) = Left (evaluate globals ls thing)
exec globals locals (Block stmts) = case execs globals locals stmts of
    Left thing -> Left thing
    Right blah -> Right locals

exec globals locals (While val stmt)
    | isTrue (evaluate globals locals val) = case exec globals locals stmt of
        Right locals' -> exec globals locals' (While val stmt)
        Left blah -> Left blah
    | otherwise = Right locals

exec globals locals (If val stmt)
    | isTrue (evaluate globals locals val) = case exec globals locals stmt of
        Right (Namespace locals') -> Right (Namespace (("@", NumberV 1):locals'))
        Left thing -> Left thing
    | otherwise = case locals of
        (Namespace locals') -> Right (Namespace (("@", NumberV 0):locals'))
exec globals locals (Elif val stmt)
    | (varEq "@" (NumberV 0) locals) && (isTrue (evaluate globals locals val)) = case exec globals locals stmt of
        Right (Namespace locals') -> Right (Namespace (("@", NumberV 1):locals'))
        Left thing -> Left thing
    | otherwise = Right locals

exec globals locals (Else stmt)
    | (varEq "@" (NumberV 0) locals) = exec globals locals stmt
    | otherwise = Right locals

exec globals (Namespace locals) (Declare name) = Right $ Namespace ((name, Void):locals)
exec globals ls@(Namespace locals) (DeclAssign name val) = Right $ Namespace $ (name, evaluate globals ls val):locals


isTrue (NumberV 0) = False
isTrue (StringV "") = False
isTrue _ = True

execs :: Namespace -> Namespace -> [Statement] -> Either Value Namespace
execs globals locals (stmt:stmts) = case exec globals locals stmt of
    Right stuff -> execs globals stuff stmts
    Left thing -> Left thing
execs globals locals [] = Right locals

declare :: Namespace -> Declaration -> Namespace
declare globals@(Namespace gs) (FuncDec str args body) = Namespace $ (str, FuncV function):gs where
    function :: Namespace -> [Value] -> Value
    function globals' argVals = case exec globals' (makeLocals argVals) body of
        Right _ -> Void
        Left val -> val
    makeLocals :: [Value] -> Namespace
    makeLocals argVals = Namespace $ zip args argVals

load :: Namespace -> Program -> Namespace
load globals (Program decls) = foldl declare globals decls

loadFromScratch :: Program -> Namespace
loadFromScratch prog = load stdEnv prog


loadScratchText text = case program text of
    Right (a, _) -> loadFromScratch a


stdEnv :: Namespace
stdEnv = Namespace []

