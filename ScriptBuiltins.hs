{-# LANGUAGE FlexibleInstances #-}
module ScriptBuiltins where

import ExecScript
import Data.List (intercalate)
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
    ("insert", adaptToVal sInsert),
    ("null", Void),
    ("bool", adaptToVal sBool),
    ("eq", adaptToVal sEquals),
    ("?=", adaptToVal sEquals),
    ("cmp", adaptToVal sCompare),
    ("<", adaptToVal sLessThan),
    (">", adaptToVal sGreaterThan),
    ("<=", adaptToVal sLessThanEquals),
    (">=", adaptToVal sGreaterThanEquals),
    ("readFile", adaptToVal sReadFile),
    ("writeFile", adaptToVal sWriteFile),
    ("join", adaptToVal sJoin),
    ("map", adaptToVal sMap),
    ("reduce", adaptToVal sReduce)]

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
sAdd [NumberV a, NumberV b] = return $ NumberV (a + b)
sAdd [StringV a, b] = return $ StringV (a ++ (toString b))
sAdd [ListV a, ListV b] = return $ ListV (a ++ b)

sMultiply :: SFunction
sMultiply [NumberV a, NumberV b] = return $ NumberV (a * b)
sMultiply [StringV a, NumberV b] = return $ StringV (concat $ replicate b a)
sMultiply [ListV a, NumberV b] = return $ ListV (concat $ replicate b a)

sDivide :: SFunction
sDivide = adapt (div :: Int -> Int -> Int)


sIndex :: SFunction
sIndex [ListV a, NumberV b] = return (a !! b)
sIndex [StringV a, NumberV b] = return $ StringV ([a !! b])

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

sBool :: SFunction
sBool [a] = return $ if isTrue a then NumberV 1 else NumberV 0


sEquals :: SFunction
sEquals [a, b] = return $ NumberV $ if (a == b) then 1 else 0

instance Ord Value where
    compare (ListV a) (ListV b) = compare a b
    compare (StringV a) (StringV b) = compare a b
    compare (NumberV a) (NumberV b) = compare a b
    compare Void Void = EQ
    compare Void a = LT
    compare a Void = GT

sCompare :: SFunction
sCompare [a, b] = return $ NumberV $ case compare a b of
    LT -> -1
    EQ -> 0
    GT -> 1

sLessThan :: SFunction
sLessThan [a, b] = return $ NumberV $ case a < b of
    True -> 1
    False -> 0

sGreaterThan :: SFunction
sGreaterThan [a, b] = return $ NumberV $ case a > b of
    True -> 1
    False -> 0

sLessThanEquals :: SFunction
sLessThanEquals [a, b] = return $ NumberV $ case a <= b of
    True -> 1
    False -> 0

sGreaterThanEquals :: SFunction
sGreaterThanEquals [a, b] = return $ NumberV $ case a >= b of
    True -> 1
    False -> 0

sReadFile :: SFunction
sReadFile [StringV fname] = fmap StringV (readFile fname)

sWriteFile :: SFunction
sWriteFile [StringV fname, StringV text] = (writeFile fname text) >> return Void
sWriteFile [StringV fname] = (writeFile fname "") >> return Void

toString :: Value -> String
toString (StringV a) = a
toString (NumberV a) = show a
toString (ListV a) = show a
toString Void = "null"

sJoin :: SFunction
sJoin [ListV a, StringV b] = return $ StringV $ intercalate b (map toString a)
sJoin [ListV a] = return $ StringV $ (concat $ map toString a)

sMap :: SFunction
sMap [FuncV f, ListV a] = f (Namespace []) (map return a)

sReduce :: SFunction
sReduce [FuncV f, ListV xs] = foldr1 thing (map return xs) where
    thing a b = f (Namespace []) [a, b]

