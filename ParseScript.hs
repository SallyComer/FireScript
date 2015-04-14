module ParseScript where

import ScanScript
import ASTData




data ParserTemplate f = Template String [ParserChoice] ([UserMade] -> f)

matchTemplate :: ParserChoice -> Parser UserMade
matchTemplate StringContents (StrT str:rest) = Right (UserString str, rest)
matchTemplate MakeName (NameT name:rest) = Right (UserIdentifier name, rest)
matchTemplate InParentheses (LParen:rest) = case parseExpr rest of
    Right (val, RParen:rest') -> Right (UserValue val, rest')
    Right _ -> Left "missing close parenthesis"
    Left a -> Left a
{-
matchTemplate key@(SpecificKeyword thing) (word:rest) | word == (KeywordT thing) = Right (UserKeyword thing, rest)
    | otherwise = Left ("expecting '" ++ thing ++ "', found '" ++ show word ++ "'")

matchTemplate sym@(SpecificOperator thing) (bol:rest) | bol == (OperatorT thing) = Right (UserSymbol thing, rest)
    | otherwise = Left ("expecting '" ++ thing ++ "', found '" ++ show bol ++ "'")
-}
matchTemplate (SpecificToken tok) (tok':rest) | tok == tok' = Right (UserToken tok', rest)
    | otherwise = Left ("expecting '" ++ show tok ++ "', found '" ++ show tok' ++ "'")

matchTemplate ParenArgs (LParen:rest) = case parseCommaStuff rest of
    Right (exprs, RParen:rest') -> Right (UserArgs exprs, rest')
    Right _ -> Left "missing close parenthesis"
    Left a -> Left a

matchTemplate MakeValue tokens = case parseExpr tokens of
    Right (val, rest) -> Right (UserValue val, rest)
    Left a -> Left a

matchTemplate MakeStatement tokens = case parseStatement tokens of
    Right (stmt, rest) -> Right (UserCommand stmt, rest)
    Left a -> Left a



accomplishTemplate :: ParserTemplate f -> Parser f
accomplishTemplate (Template key blueprint reifier) (KeywordT foo:rest) | key == foo = case parseByTemplate blueprint rest of
        Right (result, rest') -> Right (reifier result, rest')
        Left a -> Left a
    | otherwise = Left $ "didn't work " ++ foo
accomplishTemplate _ _ = Left "ain't a template form"

parseByTemplate :: [ParserChoice] -> Parser [UserMade]
parseByTemplate stuff = catParsers (map matchTemplate stuff)


catParsers :: [Parser UserMade] -> Parser [UserMade]
catParsers (p:qs) tokens@(_:_) = case p tokens of
    Right (val, rest) -> case catParsers qs rest of
        Right (vals, rest') -> Right (val:vals, rest')
        Left a -> Left a
    Left a -> Left a
catParsers [] rest = Right ([], rest)
catParsers (_:_) [] = Left "ran out of tokens!"


tryTemplates :: [ParserTemplate f] -> Parser f
tryTemplates (p:qs) tokens = case accomplishTemplate p tokens of
    Right (thing, rest) -> Right (thing, rest)
    Left _ -> tryTemplates qs tokens
tryTemplates [] tokens = Left "even the user-made templates failed"





-- Section for parser templates





-- End parser templates

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


parseExpr' (LBracket:rest) = case parseCommaStuff rest of
    Right (items, (RBracket:rest')) -> Right (List items, rest')
    a -> Left ("stuff with brackets: " ++ show a)
parseExpr' a = Left ("some screwup with expressions: " ++ show a)


parseFullExpr :: (Maybe Expr) -> Parser Expr
parseFullExpr (Just thing) (OperatorT op:toks) = case parseExpr' toks of
    Right (thing1, rest) -> case dealWithOp op thing thing1 of
        Right yes -> parseFullExpr (Just yes) rest
        Left no -> Left no
    a -> a
parseFullExpr Nothing toks = case parseExpr' toks of
    Right (thing, rest) -> parseFullExpr (Just thing) rest
    Left a -> Left a
parseFullExpr (Just (Get thing name)) (LParen:toks) = case parseExpr' (LParen:toks) of
    Right (Tuple tup, rest) -> Right (Method thing name tup, rest)
parseFullExpr (Just thing) (LParen:toks) = case parseExpr' (LParen:toks) of
    Right (Tuple tup, rest) -> Right (Call thing tup, rest)
    Right (other, _) -> Left "Syntax error: you've got to use parentheses to call"
    Left a -> Left a
parseFullExpr (Just thing) toks = Right (thing, toks)

parseExpr :: Parser Expr
parseExpr = parseFullExpr Nothing




dealWithOp :: String -> Expr -> Expr -> Either String Expr
dealWithOp "." first (Name second) = Right $ Get first second
dealWithOp "." first second = Left $ "messed up field access"
dealWithOp "::" first (Name second) = Right $ MemberAccess first second
dealWithOp "::" first second = Left $ "messed up member access"
dealWithOp op first second = Right $ Operator op first second









parseSemicolons :: Parser [Statement]
parseSemicolons (RBrace:rest) = Right ([], rest)
parseSemicolons a = case parseStatement a of
    Right (stmt, rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')
        Left a -> Left a


    (Left a) -> Left ("THING WITH SEMICOLONS: " ++ a)


parseStatement :: Parser Statement
parseStatement ((NameT name):EqT:(OperatorT op):rest) = do
    parseStatement ((NameT name):EqT:(NameT name):(OperatorT op):rest)

parseStatement ((NameT name):EqT:rest) = do
    (thing, rest') <- munchSemi $ parseExpr rest
    return (Assign name thing, rest')

parseStatement (LBrace:rest) = do
    (stmts, rest') <- parseSemicolons rest
    return (Block stmts, rest')

parseStatement (KeywordT "Var":NameT name:EqT:rest) = do
    (val, rest') <- munchSemi $ parseExpr rest
    return (DeclAssign name val, rest')

parseStatement (KeywordT "Var":NameT name:Semicolon:rest) = do
    return (Declare name, rest)

parseStatement asdf = case tryTemplates statementTemplates asdf of
    Right (thing, rest) -> Right (thing, rest)
    Left screwup -> case parseExpr asdf of
        Right (Get obj field, EqT:rest) -> case munchSemi $ parseExpr rest of
            Right (thing, rest') -> case fieldAssign (Get obj field, thing) of
                Left blah -> Right (blah, rest')
                Right blah -> Left ("got a weird thing from fieldAssign " ++ show blah)
            Left a -> Left a
        Right (val, rest) -> munchSemi $ Right (Do val, rest)
        Left a -> Left ("parseStatement: " ++ screwup ++ " IN ADDITION TO: "++ a)



statementTemplates :: [ParserTemplate Statement]

statementTemplates = [
    Template "Suspend" [MakeValue, SpecificToken Semicolon] (Command "Suspend"),
    Template "If" [InParentheses, MakeStatement] (Command "If"),
    Template "While" [InParentheses, MakeStatement] (Command "While"),
    Template "Else" [MakeStatement] (Command "Else"),
    Template "Elif" [InParentheses, MakeStatement] (Command "Elif"),
    Template "Return" [MakeValue, SpecificToken Semicolon] (Command "Return"),
    Template "Put" [MakeValue, SpecificToken Comma, MakeValue, SpecificToken Semicolon] (Command "Put"),
    Template "Kill" [MakeValue, SpecificToken Semicolon] (Command "Kill"),
    Template "Shove" [MakeValue, SpecificToken Comma, MakeValue, SpecificToken Semicolon] (Command "Shove")
    ]
munchSemi :: Show a => Either String (a, [Token]) -> Either String (a, [Token])
munchSemi (Right (a, Semicolon:rest)) = Right (a, rest)
munchSemi (Right (a, rest)) = Left $ "you're missing a semicolon: " ++ show a ++ " and here's the rest: " ++ show rest
munchSemi (Left a) = (Left a)


fieldAssign :: (Expr, Expr) -> Either Statement (Expr, Expr)
fieldAssign (Name blah, rightSide) = Left (Assign blah rightSide)
fieldAssign (Get obj field, rightSide) = fieldAssign (obj, Operator ":" (Parens obj) (List [Str field, rightSide]))
fieldAssign a = Right a

parseFuncDeclArgs :: Parser [String]
parseFuncDeclArgs (RParen:rest) = Right ([], rest)
parseFuncDeclArgs (NameT arg:RParen:rest) = Right ([arg], rest)
parseFuncDeclArgs (NameT arg:Comma:rest) = case parseFuncDeclArgs rest of
    Right (args, rest') -> Right (arg:args, rest')
    Left a -> Left a
parseFuncDeclArgs a = Left ("parseDeclargs " ++ show a)

parseDecl :: Parser Declaration
parseDecl (KeywordT "Def":NameT name:LParen:rest) = case parseFuncDeclArgs rest of
    Right (args, rest') -> case parseStatement rest' of
        Right (body, rest'') -> Right (FuncDec name args body, rest'')
        Left a -> Left a
    Left a -> Left a
parseDecl (KeywordT "Class":NameT name:LBrace:rest) = case parseDecls rest of
    Right (decls, RBrace:rest') -> Right (ClassDec name decls, rest')
    Right _ -> Left ("The class declaration of '" ++ name ++ "' needs a closing brace")
    Left a -> Left a
parseDecl (KeywordT "Var":NameT name:rest) = case munchSemi $ parseExpr rest of
    Right (val, rest') -> Right (VarDec name val, rest')
    Left a -> Left a
parseDecl (KeywordT "Module":NameT modName:LBrace:rest) = case parseProgram rest of
    Right (prgm, RBrace:rest') -> Right (ModDec modName prgm, rest')
    Left a -> Left a
parseDecl (KeywordT "Operator":NameT arg1:OperatorT op:NameT arg2:rest) = case parseStatement rest of
    Right (body, rest') -> Right (FuncDec op [arg1, arg2] body, rest')
    Left a -> Left a
parseDecl a = Left ("parseDecl screwed up:\n" ++ show a)

parseDecls :: Parser [Declaration]
parseDecls [] = Right ([], [])
parseDecls (RBrace:rest) = Right ([], RBrace:rest)
parseDecls blah = case parseDecl blah of
    Right (decl, rest) -> case parseDecls rest of
        Right (decls, rest') -> Right (decl:decls, rest')
        Left a -> Left a
    Left a -> Left a

parseProgram :: Parser Program
parseProgram [] = Right (Program [], [])

parseProgram a = case parseDecls a of
    Right (decls, rest) -> Right (Program decls, rest)
    Left a -> Left a

expression = parseExpr . tokenize
statement = parseStatement . tokenize
decl = parseDecl . tokenize
program = parseProgram . tokenize