module ExecScript where
import ParseScript
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (void, foldM)


data Value = NumberV Int
    | StringV String
    | ListV [Value]
    | FuncV FuncVT
    | Void
    | ErrorV String
    | ObjectV Namespace
    | Ember (MVar Value, ThreadId)
    | Module Namespace


type FuncVT = Namespace -> [IO Value] -> IO Value
type SFunction = Namespace -> [Value] -> IO Value
adaptToVal :: SFunction -> Value
adaptToVal func = FuncV (\globals args -> (flipListIO args) >>= (func globals))

instance Eq Value where
    (StringV a) == (StringV b) = a == b
    (ListV a) == (ListV b) = a == b
    (NumberV a) == (NumberV b) = a == b
    Void == Void = True
    _ == _ = False



instance Show Value where
    show (NumberV i) = show i
    show (StringV s) = show s
    show (ListV l) = show l
    show Void = "null"
    show (FuncV _) = "<function>"
    show (Ember _) = "A burning hole in the world"

data Namespace = Namespace [(String, Value)] deriving (Show)

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
    Left a -> error ("Looked for name that is not in:\n"++ show scope)

unEither (Right a) = a
unEither (Left a) = error (show a)

evaluate :: Namespace -> Namespace -> Expr -> IO Value
evaluate globals locals (MemberAccess thing mem) = fmap (getAttr mem) (evaluate globals locals thing)
evaluate globals locals (Spark comp) = do
    ember <- newEmptyMVar
    tId <- forkIO ((evaluate globals locals comp) >>= (\a -> putMVar ember a))
    return (Ember (ember, tId))
evaluate globals locals (Take e) = (evaluate globals locals e) >>= (\a -> case a of
    Ember (e', _) -> takeMVar e')
evaluate globals locals (Read e) = (evaluate globals locals e) >>= (\a -> case a of
    Ember (e', _) -> readMVar e')
evaluate globals locals Ignite = fmap Ember ((,) <$> newEmptyMVar <*> myThreadId)
evaluate globals locals (Number i) = return $ NumberV i
evaluate globals locals (Str s) = return $ StringV s
evaluate globals locals (Parens thing) = evaluate globals locals thing
evaluate globals locals (Operator op thing1 thing2) = callOperator globals op (evaluate globals locals thing1) (evaluate globals locals thing2)
evaluate globals locals (Call func stuff) = (evaluate globals locals func) >>= (\a -> callFunction globals a (map (evaluate globals locals) stuff))
evaluate globals locals (List stuff) = (fmap ListV) (flipListIO $ map (evaluate globals locals) stuff)
evaluate globals locals (Name blah) = return $ unEither $ fallbackSearch blah locals globals
evaluate globals locals (Get thing str) = fmap (getAttr str) (evaluate globals locals thing)
evaluate globals locals (Method thing name args) = (evaluate globals locals thing) >>= (\a ->
    callMethod globals a (getAttr name a) (map (evaluate globals locals) args))


getAttr :: String -> Value -> Value
getAttr str val = unsafeSearch str (getVars val)

getVars :: Value -> Namespace
getVars (ObjectV o) = o
getVars (ListV _) = listClass
getVars (StringV _) = strClass
getVars (NumberV _) = numClass
getVars (Module m) = m

listClass :: Namespace
listClass = Namespace [
    ("length", adaptToVal listLength),
    ("indexOf", adaptToVal listIndexOf)]


listLength :: SFunction
listLength globals [ListV a] = return $ NumberV $ length a

listIndexOf :: SFunction
listIndexOf globals [ListV a, b] = return $ NumberV $ length (takeWhile (/= b) a)

sUpdateName :: SFunction
sUpdateName globals [ObjectV (Namespace foo), ListV [StringV str, a]] = return $ ObjectV (Namespace ((str, a):foo))

strClass :: Namespace
strClass = Namespace []

numClass :: Namespace
numClass = Namespace []

--createFunction :: Declaration -> [Value] -> Value
--createFunction (FuncDec _ args body) = undefined

callOperator :: Namespace -> String -> IO Value -> IO Value -> IO Value
callOperator globals name arg1 arg2 = callFunction globals (unsafeSearch name globals) [arg1, arg2]

callFunction :: Namespace -> Value -> [IO Value] -> IO Value
callFunction globals (FuncV func) args = func globals args
callFunction globals (ObjectV obj) args | nameExists obj "__call__" = case unsafeSearch "__call__" obj of
        FuncV func -> func globals ((return $ ObjectV obj):args)
    | otherwise = error ("object does not have a .__call__ method: " ++ show obj)

callMethod :: Namespace -> Value -> Value -> [IO Value] -> IO Value
callMethod globals obj func args = callFunction globals func (return obj:args)


exec :: Namespace -> Namespace -> Statement -> IO (Either Value Namespace)
exec globals ls@(Namespace locals) (Assign str val) = fmap (\a -> Right $ Namespace $ (str, a):locals) (evaluate globals ls val)
exec globals ls@(Namespace locals) (Do val) = fmap (\a -> Right $ Namespace $ ("_", a):locals) (evaluate globals ls val)
exec globals ls@(Namespace locals) (Return thing) = fmap Left (evaluate globals ls thing)

exec globals locals (Block stmts) = fmap (\a -> case a of
    Left thing -> Left thing
    Right blah -> Right $ updateNames locals blah) (execs globals locals stmts)

exec globals locals (While val stmt) = (evaluate globals locals val) >>= (\a -> if isTrue a then condTrue' else condFalse) where
    condTrue (Right locals') = exec globals locals' (While val stmt)
    condTrue (Left blah) = return (Left blah)
    condTrue' = (exec globals locals stmt) >>= condTrue
    condFalse = return (Right locals)


exec globals locals (If val stmt) = (evaluate globals locals val) >>= (\a -> if isTrue a then condTrue else condFalse) where
    condTrue = (exec globals locals stmt) >>= (\blah -> return $ case blah of
        Right (Namespace locals') -> Right (Namespace (("@", NumberV 1):locals'))
        Left thing -> Left thing)
    condFalse = return $ case locals of
        (Namespace locals') -> Right (Namespace (("@", NumberV 0):locals'))


exec globals locals (Elif val stmt) = (evaluate globals locals val) >>= (\a -> if ((varEq "@" (NumberV 0) locals) && isTrue a) then condTrue else condFalse) where
    condTrue = (exec globals locals stmt) >>= (\blah -> return $ case blah of
        Right (Namespace locals') -> Right (Namespace (("@", NumberV 1):locals'))
        Left thing -> Left thing)
    condFalse = return $ Right locals

exec globals locals (Else stmt)
    | (varEq "@" (NumberV 0) locals) = exec globals locals stmt
    | otherwise = return $ Right locals



exec globals (Namespace locals) (Declare name) = return $ Right $ Namespace ((name, Void):locals)
exec globals ls@(Namespace locals) (DeclAssign name val) = fmap (\a -> Right $ Namespace $ (name, a):locals) (evaluate globals ls val)

exec globals locals (Put ember val) = do
    val' <- evaluate globals locals val
    ember' <- evaluate globals locals ember
    case ember' of
        Ember (e', _) -> putMVar e' val'
    return (Right locals)

isTrue (NumberV 0) = False
isTrue (StringV "") = False
isTrue _ = True

execs :: Namespace -> Namespace -> [Statement] -> IO (Either Value Namespace)
execs globals locals (stmt:stmts) = (exec globals locals stmt) >>= (\a -> case a of
    Right stuff -> execs globals stuff stmts
    Left thing -> return $ Left thing) 
execs globals locals [] = return $ Right locals

declare :: Namespace -> Declaration -> IO Namespace
declare globals@(Namespace gs) (FuncDec str args body) = return $ Namespace $ (str, FuncV function):gs where
    function :: FuncVT
    function globals' argVals = fmap (\a -> case a of
        Right _ -> Void
        Left val -> val) ((makeLocals argVals) >>= (\b -> exec globals' b body))
    makeLocals :: [IO Value] -> IO Namespace
    makeLocals argVals = fmap (\a -> Namespace $ zip args a) (flipListIO argVals)

declare globals@(Namespace gs) (ClassDec str decls) = (\cn -> Namespace $ (str, adaptToVal $ constructor' cn):gs) <$> classNames where
    classNames = foldM declare globals decls
    constructor' :: Namespace -> SFunction
    constructor' classNames' globals' args | nameExists classNames' "__init__" = callFunction globals' (unsafeSearch "__init__" classNames') ((map return) $ ObjectV classNames':args)
        | otherwise = return $ ObjectV classNames'
    
declare globals@(Namespace gs) (VarDec str expr) = (\v -> Namespace $ (str, v):gs) <$> (evaluate globals (Namespace []) expr)
declare globals@(Namespace gs) (ModDec name body) = (\m -> Namespace $ (name, Module m):gs) <$> (load globals body)


flipListIO :: [IO a] -> IO [a]
flipListIO [] = return []
flipListIO (x:xs) = x >>= (\a -> fmap (a:) (flipListIO xs)) 

load :: Namespace -> Program -> IO Namespace
load globals (Program imports decls) = do
    globals' <- foldM obtainProgram globals imports
    foldM declare globals' decls

loadFromScratch :: Program -> IO Namespace
loadFromScratch prog = load (Namespace []) prog


obtainProgram :: Namespace -> FilePath -> IO Namespace
obtainProgram globals fname = do
    text <- readFile fname
    importProgram globals text

loadScratchText text = case program text of
    Right (a, _) -> loadFromScratch a

runMain :: Namespace -> IO Value
runMain globals = do
    ret <- evaluate globals (Namespace []) (Call (Name "main") [])
    yield
    return ret


runText :: String -> IO Value
runText text = (loadScratchText text) >>= runMain

importProgram :: Namespace -> String -> IO Namespace
importProgram globals text = case program text of
    Right (a, _) -> load globals a


