module ASTData where

import ScanScript

data Expr = Number Int
    | Str String
    | Parens Expr
    | Operator String Expr Expr
    | Call Expr [Expr]
    | List [Expr]
    | Name String 
    | Get Expr String
    | Method Expr String [Expr]
    | Take Expr
    | Spark Expr
    | Read Expr
    | Ignite
    | MemberAccess Expr String
    | Lambda [String] Statement
    | Form String [UserMade]
    | Tuple [Expr] deriving (Show, Eq)



data Statement = Assign String Expr
    | Do Expr
    | Block [Statement]
--    | While Expr Statement
--    | If Expr Statement
--    | Else Statement
--    | Elif Expr Statement
    | Declare String
    | DeclAssign String Expr
--    | Return Expr
--    | Put Expr Expr
--    | Shove Expr Expr
--    | Kill Expr
    | Command String [UserMade] deriving (Show, Eq)


data Declaration = FuncDec String [String] Statement
    | ClassDec String [Declaration] 
    | VarDec String Expr
    | ModDec String Program
    | Import String deriving (Show)


data Program = Program [Declaration] deriving (Show)

type Parser a = [Token] -> Either String (a, [Token])




data UserMade = UserString String
    | UserIdentifier String
    | UserValue Expr
    | UserCommand Statement
    | UserKeyword String
    | UserSymbol String
    | UserArgs [Expr]
    | UserToken deriving (Show, Eq)


data ParserChoice = StringContents
    | MakeName
    | MakeValue
    | MakeStatement
    | InParentheses
    | ParenArgs
    | SpecificKeyword String
    | SpecificOperator String
    | SpecificToken Token