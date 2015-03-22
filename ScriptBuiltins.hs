{-# LANGUAGE FlexibleInstances #-}
module ScriptBuiltins where

import ExecScript

stdEnv :: Namespace
stdEnv = Namespace [
    ("print", adaptToVal sPrint),
    ("+", adaptToVal sAdd),
    ("-", adaptToVal sSubtract),
    ("/", adaptToVal sDivide),
    ("*", adaptToVal sMultiply),
    ("plus", adaptToVal sAdd),
    ("minus", adaptToVal sSubtract),
    ("div", adaptToVal sDivide),
    ("mul", adaptToVal sMultiply),
    ("!!", adaptToVal sIndex),
    ("index", adaptToVal sIndex),
    ("cons", adaptToVal sCons),
    (":", adaptToVal sCons),
    ("head", adaptToVal sHead),
    ("tail", adaptToVal sTail),
    ("init", adaptToVal sInit),
    ("last", adaptToVal sLast),
    ("set", adaptToVal sSet),
    ("insert", adaptToVal sInsert)]

type SFunction = [Value] -> IO Value

adaptToVal :: SFunction -> Value
adaptToVal func = FuncV (\globals args -> (flipListIO args) >>= func)



class Adaptable f where
    adapt :: f -> SFunction



sPrint :: SFunction
sPrint [StringV str] = putStrLn str >> return Void
sPrint [ErrorV a] = return (ErrorV a)
sPrint [a] = print a >> return Void
sPrint a = return (ErrorV (show a))

instance Adaptable (Int -> Int -> Int) where
    adapt f [NumberV a, NumberV b] = return (NumberV (f a b))

sSubtract :: SFunction
sSubtract = adapt ((-) :: Int -> Int -> Int)

sAdd :: SFunction
sAdd = adapt ((+) :: Int -> Int -> Int)

sMultiply :: SFunction
sMultiply = adapt ((*) :: Int -> Int -> Int)

sDivide :: SFunction
sDivide = adapt (div :: Int -> Int -> Int)


sIndex :: SFunction
sIndex [ListV a, NumberV b] = return (a !! b)
sIndex a = return (ErrorV (show a))

sCons :: SFunction
sCons [a, ListV b] = return (ListV (a:b))

sHead :: SFunction
sHead [ListV a] = return $ head a

sTail :: SFunction
sTail [ListV a] = return $ ListV $ tail a

sInit :: SFunction
sInit [ListV a] = return $ ListV $ init a

sLast :: SFunction
sLast [ListV a] = return $ last a

set :: [a] -> Int -> a -> [a]
set xs i val = (take i xs) ++ (val:(drop (succ i) xs))

insert :: [a] -> Int -> a -> [a]
insert xs i val = (take i xs) ++ [val] ++ (drop i xs)

sSet :: SFunction
sSet [ListV xs, NumberV i, val] = return $ ListV $ set xs i val

sInsert :: SFunction
sInsert [ListV xs, NumberV i, val] = return $ ListV $ insert xs i val


