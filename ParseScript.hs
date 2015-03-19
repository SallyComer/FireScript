module ParseScript where

import ScanScript


data Statement = Assign String Expr
    | Do Expr
    | Block [Statement]
    | While Expr Statement
    | If Expr Statement
    | Else Statement
    | Elif Statement
    | For Statement Expr Statement
    | Declare String
    | DeclAssign String Expr
    | Return Expr deriving (Show, Eq)



data Expr = Number Int
    | Str String
    | Parens Expr
    | Operator String Expr Expr
    | Call String [Expr]
    | List [Expr]
    | Name String deriving (Show, Eq)




data Declaration = FuncDec String [String] [Statement]



data Program = Program [Declaration]


type Parser a = [Token] -> Either String (a, [Token])


parseCommaStuff :: Parser [Expr]
parseCommaStuff a = case parseExpr a of
    Right (thing, (Comma:rest)) -> case parseCommaStuff rest of
        Right (things, rest') -> Right (thing:things, rest')
    Right (thing, rest) -> Right ([thing], rest)
    Left _ -> Right ([], a)

parseExpr :: Parser Expr
parseExpr ((NameT name):LParen:rest) = parseOp $ case parseCommaStuff rest of
    Right (args, (RParen:rest')) -> Right (Call name args, rest')
parseExpr (NumberT a:rest) = parseOp $ Right (Number a, rest)
parseExpr (NameT a:rest) = parseOp $ Right (Name a, rest)
parseExpr (StrT a:rest) = parseOp $ Right (Str a, rest)
parseExpr (LParen:rest) = parseOp $ case parseExpr rest of
    Right (thing, (RParen:rest')) -> Right (Parens thing, rest')
parseExpr (LBracket:rest) = parseOp $ case parseCommaStuff rest of
    Right (items, (RBracket:rest')) -> Right (List items, rest')
    a -> Left (show a)
parseExpr a = Left (show a)

parseOp (Right (arg1, (OperatorT op:rest))) = case parseExpr rest of
    Right (arg2, rest') -> Right (Operator op arg1 arg2, rest')
parseOp a = a

parseText = parseExpr . tokenize