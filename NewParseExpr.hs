

import ASTData
import ScanScript
import ParseScript hiding (parseExpr, parseCommaStuff)






parseCommaStuff :: Parser [Expr]
parseCommaStuff (RParen:rest) = Right ([], RParen:rest)
parseCommaStuff (RBracket:rest) = Right ([], RBracket:rest)
parseCommaStuff a = case parseFullExpr Nothing a of
    Right (thing, (Comma:rest)) -> case parseCommaStuff rest of
        Right (things, rest') -> Right (thing:things, rest')
    Right (thing, rest) -> Right ([thing], rest)
    Left thing -> Left thing

parseExpr' :: Parser Expr
{-
parseExpr' stuff@(NameT name_:OperatorT "::":rest_) = Right $ (\(a, b) -> (b, a)) (fmap makeMemberAccess (extentOfColon stuff)) where
    extentOfColon :: [Token] -> ([Token], [String])
    extentOfColon (NameT name:OperatorT "::":rest) = case extentOfColon rest of
        (rest', things) -> (rest', name:things)
    extentOfColon (NameT name:rest) = (rest, [name])
    makeMemberAccess :: [String] -> Expr
    makeMemberAccess [foo] = Name foo
    makeMemberAccess foo = MemberAccess (makeMemberAccess (init foo)) (last foo)
-}

parseExpr' (KeywordT "Lambda":LParen:rest) = do
    (args, rest') <- parseFuncDeclArgs rest
    (body, rest'') <- parseStatement rest'
    return (Lambda args body, rest'')

parseExpr' (KeywordT "Ignite":rest) = Right (Ignite, rest)

parseExpr' (KeywordT "Spark":rest) = do
    (thing, rest') <- parseFullExpr Nothing rest
    return (Spark thing, rest')

parseExpr' (KeywordT "Take":rest) = do
    (thing, rest') <- parseFullExpr Nothing rest
    return (Take thing, rest')

parseExpr' (KeywordT "Read":rest) = do
    (thing, rest') <- parseFullExpr Nothing rest
    return (Read thing, rest')


parseExpr' (LParen:rest) = case parseCommaStuff rest of
    Right (args, (RParen:rest')) -> Right (Tuple args, rest')
    Right (args, rest') -> Left $ "You've got some missing close parens"
    Left a -> Left a

parseExpr' (NumberT a:rest) = Right (Number a, rest)
parseExpr' (NameT a:rest) = Right (Name a, rest)
parseExpr' (StrT a:rest) = Right (Str a, rest)


parseExpr' (LBracket:rest) = parseOp $ case parseCommaStuff rest of
    Right (items, (RBracket:rest')) -> Right (List items, rest')
    a -> Left ("stuff with brackets: " ++ show a)
parseExpr' a = Left ("some screwup with expressions: " ++ show a)


parseFullExpr :: (Maybe Expr) -> Parser Expr
parseFullExpr (Just thing) (OperatorT op:toks) = case parseExpr' toks of
    Right (thing1, rest) -> parseFullExpr (Just $ Operator op thing thing1) rest
    a -> a
parseFullExpr Nothing toks = case parseExpr' toks of
    Right (thing, rest) -> parseFullExpr (Just thing) rest
    Left a -> Left a
parseFullExpr (Just thing) (LParen:toks) = case parseExpr' (LParen:toks) of
    Right (Tuple tup, rest) -> Right (Call thing tup, rest)
    Right (other, _) -> Left "Syntax error: you've got to use parentheses to call"
    Left a -> Left a
parseFullExpr (Just thing) toks = Right (thing, toks)