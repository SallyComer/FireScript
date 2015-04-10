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
matchTemplate (SpecificToken tok) (tok':rest) | tok == tok' = Right (UserToken, rest)
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


{- BEGINNING OF EXCISION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseCommaStuff :: Parser [Expr]
parseCommaStuff (RParen:rest) = Right ([], RParen:rest)
parseCommaStuff (RBracket:rest) = Right ([], RBracket:rest)
parseCommaStuff a = case parseExpr a of
    Right (thing, (Comma:rest)) -> case parseCommaStuff rest of
        Right (things, rest') -> Right (thing:things, rest')
    Right (thing, rest) -> Right ([thing], rest)
    Left thing -> Left thing

parseExpr :: Parser Expr
parseExpr stuff@(NameT name_:OperatorT "::":rest_) = parseOp $ Right $ (\(a, b) -> (b, a)) (fmap makeMemberAccess (extentOfColon stuff)) where
    extentOfColon :: [Token] -> ([Token], [String])
    extentOfColon (NameT name:OperatorT "::":rest) = case extentOfColon rest of
        (rest', things) -> (rest', name:things)
    extentOfColon (NameT name:rest) = (rest, [name])
    makeMemberAccess :: [String] -> Expr
    makeMemberAccess [foo] = Name foo
    makeMemberAccess foo = MemberAccess (makeMemberAccess (init foo)) (last foo)

parseExpr (KeywordT "Lambda":LParen:rest) = do
    (args, rest') <- parseFuncDeclArgs rest
    (body, rest'') <- parseStatement rest'
    return (Lambda args body, rest'')

parseExpr (KeywordT "Ignite":rest) = parseOp $ Right (Ignite, rest)

parseExpr (KeywordT "Spark":rest) = parseOp $ do
    (thing, rest') <- parseExpr rest
    return (Spark thing, rest')

parseExpr (KeywordT "Take":rest) = parseOp $ do
    (thing, rest') <- parseExpr rest
    return (Take thing, rest')

parseExpr (KeywordT "Read":rest) = parseOp $ do
    (thing, rest') <- parseExpr rest
    return (Read thing, rest')


parseExpr ((NameT name):LParen:rest) = parseOp $ case parseCommaStuff rest of
    Right (args, (RParen:rest')) -> Right (Call (Name name) args, rest')
    Right (args, rest') -> Left $ "You've got some missing close parens in the DECL of " ++ name
    Left a -> Left a

parseExpr (NumberT a:rest) = parseOp $ Right (Number a, rest)
parseExpr (NameT a:rest) = parseOp $ Right (Name a, rest)
parseExpr (StrT a:rest) = parseOp $ Right (Str a, rest)

parseExpr (LParen:rest) = parseOp $ case (parseOp $ parseExpr rest) of
    Right (thing, (RParen:rest')) -> Right (Parens thing, rest')
    Right (thing, a) -> Left (" Missing a closing parentheses:\n  " ++ show thing ++ show a ++ " ")
    Left a -> Left a
parseExpr (LBracket:rest) = parseOp $ case parseCommaStuff rest of
    Right (items, (RBracket:rest')) -> Right (List items, rest')
    a -> Left ("stuff with brackets: " ++ show a)
parseExpr a = Left ("some screwup with expressions: " ++ show a)

parseOp (Right (a, (OperatorT ".":NameT field:LParen:rest))) = parseOp $ case parseCommaStuff rest of
    Right (args, (RParen:rest')) -> Right (Method a field args, rest')

parseOp (Right (a, (OperatorT ".":NameT field:rest))) = parseOp $ Right (Get a field, rest)



parseOp (Right (arg1, (OperatorT op:rest))) = case parseExpr rest of
    Right (arg2, rest') -> Right (Operator op arg1 arg2, rest')
    Left a -> Left a
parseOp (Right (func, (LParen:rest))) = case parseCommaStuff rest of
    Right (args, RParen:rest') -> Right (Call func args, rest')
    a -> error ("freakin' parseOp, always screwing things up!: " ++ show a)
parseOp a = a

END OF EXCISION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-}









parseSemicolons :: Parser [Statement]
parseSemicolons (RBrace:rest) = Right ([], rest)
parseSemicolons a = case parseStatement a of
    Right (stmt, rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')
        Left a -> Left a


    a -> Left ("THING WITH SEMICOLONS: " ++ show a)


parseStatement :: Parser Statement
parseStatement ((NameT name):EqT:(OperatorT op):rest) = do
    parseStatement ((NameT name):EqT:(NameT name):(OperatorT op):rest)

parseStatement ((NameT name):EqT:rest) = do
    (thing, rest') <- munchSemi $ parseExpr rest
    return (Assign name thing, rest')

parseStatement (LBrace:rest) = do
    (stmts, rest') <- parseSemicolons rest
    return (Block stmts, rest')
{-
parseStatement (KeywordT "If":rest) = do
    (cond, rest') <- parseExpr rest
    (stmt, rest'') <- parseStatement rest'
    return (If cond stmt, rest'')

parseStatement (KeywordT "While":rest) = do
    (cond, rest') <- parseExpr rest
    (stmt, rest'') <- parseStatement rest'
    return (While cond stmt, rest'')

parseStatement (KeywordT "Else":rest) = do
    (stmt, rest') <- parseStatement rest
    return (Else stmt, rest')

parseStatement (KeywordT "Elif":rest) = do
    (cond, rest') <- parseExpr rest
    (stmt, rest'') <- parseStatement rest'
    return (Elif cond stmt, rest'')
-}
parseStatement (KeywordT "Var":NameT name:EqT:rest) = do
    (val, rest') <- munchSemi $ parseExpr rest
    return (DeclAssign name val, rest')

parseStatement (KeywordT "Var":NameT name:Semicolon:rest) = do
    return (Declare name, rest)
{-
parseStatement (KeywordT "Return":rest) = do
    (val, rest') <- munchSemi $ parseExpr rest
    return (Return val, rest')

parseStatement (KeywordT "Put":rest) = do
    (thing1, rest') <- parseExpr rest
    (thing2, rest'') <- munchSemi $ parseExpr rest'
    return (Put thing1 thing2, rest'')

parseStatement (KeywordT "Shove":rest) = do
    (thing1, rest') <- parseExpr rest
    (thing2, rest'') <- munchSemi $ parseExpr rest'
    return (Shove thing1 thing2, rest'')

parseStatement (KeywordT "Kill":rest) = do
    (thing, rest') <- munchSemi $ parseExpr rest
    return (Kill thing, rest')
-}
parseStatement a = munchSemi $ case tryTemplates statementTemplates a of
    Right (thing, rest) -> Right (thing, rest)
    Left _ -> case parseExpr a of
        Right (Get obj field, EqT:rest) -> case parseExpr rest of
            Right (thing, rest') -> case fieldAssign (Get obj field, thing) of
                Left blah -> Right (blah, rest')
                Right blah -> Left ("got a weird thing from fieldAssign " ++ show blah)
            Left a -> Left a
        Right (val, rest) -> Right (Do val, rest)
        a -> Left ("parseStatement: " ++ show a)



statementTemplates :: [ParserTemplate Statement]

statementTemplates = [
    Template "Suspend" [MakeValue] (Command "Suspend"),
    Template "If" [InParentheses, MakeStatement] (Command "If"),
    Template "While" [InParentheses, MakeStatement] (Command "While"),
    Template "Else" [MakeStatement] (Command "Else"),
    Template "Elif" [InParentheses, MakeStatement] (Command "Elif"),
    Template "Return" [MakeValue] (Command "Return"),
    Template "Put" [MakeValue, SpecificToken Comma, MakeValue] (Command "Put"),
    Template "Kill" [MakeValue] (Command "Kill"),
    Template "Shove" [MakeValue, SpecificToken Comma, MakeValue] (Command "Shove")
    ]
munchSemi :: Either String (a, [Token]) -> Either String (a, [Token])
munchSemi (Right (a, Semicolon:rest)) = Right (a, rest)
munchSemi (Right (a, rest)) = Left "you're missing a semicolon!"
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
parseDecl (KeywordT "Var":NameT name:rest) = case parseExpr rest of
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