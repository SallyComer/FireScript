module ParseScript where

import ScanScript


data Statement = Assign String Expr
    | Do Expr
    | Keyword Expr deriving (Show, Eq)



data Expr = Number Int
    | Str String
    | Parens Expr
    | Operator Expr String Expr
    | Call String [Expr]
    | List [Expr]
    | Name String deriving (Show, Eq)



data Declaration = FuncDec String [String] [Statement]



data Program = Program [Declaration]


type Parser a = [Token] -> Maybe (a, [Token])


parseCommaStuff :: Parser [Expr]
parseCommaStuff a = case parseExpr a of
    Just (thing, (Comma:rest)) -> case parseCommaStuff rest of
        Just (things, rest') -> Just (thing:things, rest')
    Nothing -> Just ([], a)

parseExpr :: Parser Expr
parseExpr (a:(OperatorT op):rest) =
    let
        Just (arg1, _) = parseExpr [a]
        Just (arg2, rest2) = parseExpr rest
    in Just (Operator arg1 op arg2, rest2)
parseExpr ((NameT name):LParen:rest) = case parseCommaStuff rest of
    Just (args, (RParen:rest')) -> Just (Call name args, rest')
parseExpr (NameT a:rest) = Just (Name a, rest)
parseExpr (StrT a:rest) = Just (Str a, rest)
parseExpr (LParen:rest) = case parseExpr rest of
    Just (thing, (RParen:rest')) -> Just (Parens thing, rest')
parseExpr (LBracket:rest) = case parseCommaStuff rest of
    Just (items, (RBracket:rest')) -> Just (List items, rest')