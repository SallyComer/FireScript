

import ASTData
import ScanScript
import ParseScript


data Intermediate = Number' Int
    | Str' String
    | Parens' [Intermediate]
    | List' [Intermediate]
    | Term' [Intermediate]
    | Name' String 
    | Form' String [UserMade]
    | Op String deriving (Show, Eq)


    
parseCommaThings :: Parser [Intermediate]
parseCommaThings (RParen:rest) = Right ([], RParen:rest)
parseCommaThings (RBracket:rest) = Right ([], RBracket:rest)
parseCommaThings a = case parseInter a of
    Right (thing, (Comma:rest)) -> case parseCommaThings rest of
        Right (things, rest') -> Right ((Term' thing):things, rest')
    Right (thing, rest) -> Right ([Term' thing], rest)
    Left thing -> Left thing





parseExpr' :: Parser Intermediate


parseExpr' (NumberT a:rest) = Right (Number' a, rest)
parseExpr' (NameT a:rest) = Right (Name' a, rest)
parseExpr' (StrT a:rest) = Right (Str' a, rest)
parseExpr' (OperatorT op:rest) = Right (Op op, rest)
parseExpr' (LParen:rest) = case (parseCommaThings rest) of
    Right (thing, (RParen:rest')) -> Right (Parens' thing, rest')
    Right (thing, a) -> Left (" Missing a closing parentheses:\n  " ++ show thing ++ show a ++ " ")
    Left a -> Left a
parseExpr' (LBracket:rest) = case parseCommaThings rest of
    Right (items, (RBracket:rest')) -> Right (List' items, rest')
    a -> Left ("stuff with brackets: " ++ show a)
parseExpr' a = Left ("some screwup with expressions: " ++ show a)

parseInter :: Parser [Intermediate]
parseInter (RParen:rest) = Right ([], RParen:rest)
parseInter (RBracket:rest) = Right ([], RBracket:rest)
parseInter (Comma:rest) = Right ([], Comma:rest)
parseInter rest = do
    (x, rest') <- parseExpr' rest
    (xs, rest'') <- parseInter rest'
    return (x:xs, rest'')