module ParseScript where

import ScanScript
import ASTData








data ParserChoice = StringContents
    | MakeName
    | MakeValue
    | MakeStatement
    | InParentheses
    | ParenArgs
    | SpecificKeyword String
    | SpecificOperator String

data ParserTemplate f = Template String [ParserChoice] ([UserMade] -> f)

matchTemplate :: ParserChoice -> Parser UserMade
matchTemplate StringContents (StrT str:rest) = Right (UserString str, rest)
matchTemplate MakeName (NameT name:rest) = Right (UserIdentifier name, rest)
matchTemplate InParentheses (LParen:rest) = case parseExpr rest of
    Right (val, RParen:rest') -> Right (UserValue val, rest')
    Right _ -> Left "missing close parenthesis"
    Left a -> Left a
matchTemplate key@(SpecificKeyword thing) (word:rest) | word == (KeywordT thing) = Right (UserKeyword thing, rest)
    | otherwise = Left ("expecting '" ++ thing ++ "', found '" ++ show word ++ "'")

matchTemplate sym@(SpecificOperator thing) (bol:rest) | bol == (OperatorT thing) = Right (UserSymbol thing, rest)
    | otherwise = Left ("expecting '" ++ thing ++ "', found '" ++ show bol ++ "'")

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




parseCommaStuff :: Parser [Expr]
parseCommaStuff a = case parseExpr a of
    Right (thing, (Comma:rest)) -> case parseCommaStuff rest of
        Right (things, rest') -> Right (thing:things, rest')
    Right (thing, rest) -> Right ([thing], rest)
    Left _ -> Right ([], a)

parseExpr :: Parser Expr
parseExpr stuff@(NameT name_:OperatorT "::":rest_) = parseOp $ Right $ (\(a, b) -> (b, a)) (fmap makeMemberAccess (extentOfColon stuff)) where
    extentOfColon :: [Token] -> ([Token], [String])
    extentOfColon (NameT name:OperatorT "::":rest) = case extentOfColon rest of
        (rest', things) -> (rest', name:things)
    extentOfColon (NameT name:rest) = (rest, [name])
    makeMemberAccess :: [String] -> Expr
    makeMemberAccess [foo] = Name foo
    makeMemberAccess foo = MemberAccess (makeMemberAccess (init foo)) (last foo)
parseExpr (KeywordT "Lambda":LParen:rest) = case parseFuncDeclArgs rest of
    Right (args, rest') -> case parseStatement rest' of
        Right (body, rest'') -> Right (Lambda args body, rest'')
parseExpr (KeywordT "Ignite":rest) = parseOp $ Right (Ignite, rest)
parseExpr (KeywordT "Spark":rest) = parseOp $ case parseExpr rest of
    Right (thing, rest') -> Right (Spark thing, rest')
parseExpr (KeywordT "Take":rest) = parseOp $ case parseExpr rest of
    Right (thing, rest') -> Right (Take thing, rest')
parseExpr (KeywordT "Read":rest) = parseOp $ case parseExpr rest of
    Right (thing, rest') -> Right (Read thing, rest')
parseExpr ((NameT name):LParen:rest) = parseOp $ case parseCommaStuff rest of
    Right (args, (RParen:rest')) -> Right (Call (Name name) args, rest')
parseExpr (NumberT a:rest) = parseOp $ Right (Number a, rest)
parseExpr (NameT a:rest) = parseOp $ Right (Name a, rest)
parseExpr (StrT a:rest) = parseOp $ Right (Str a, rest)
parseExpr (LParen:rest) = parseOp $ case (parseOp $ parseExpr rest) of
    Right (thing, (RParen:rest')) -> Right (Parens thing, rest')
    Right (thing, a) -> error ("Missing a closing parentheses:\n" ++ show a ++ "\n\n" ++ show thing)
parseExpr (LBracket:rest) = parseOp $ case parseCommaStuff rest of
    Right (items, (RBracket:rest')) -> Right (List items, rest')
    a -> Left ("stuff with brackets: " ++ show a)
parseExpr a = Left ("some screwup with expressions: " ++ show a)

parseOp (Right (a, (OperatorT ".":NameT field:LParen:rest))) = parseOp $ case parseCommaStuff rest of
    Right (args, (RParen:rest')) -> Right (Method a field args, rest')

parseOp (Right (a, (OperatorT ".":NameT field:rest))) = parseOp $ Right (Get a field, rest)



parseOp (Right (arg1, (OperatorT op:rest))) = case parseExpr rest of
    Right (arg2, rest') -> Right (Operator op arg1 arg2, rest')
parseOp (Right (func, (LParen:rest))) = case parseCommaStuff rest of
    Right (args, RParen:rest') -> Right (Call func args, rest')
parseOp a = a


parseSemicolons :: Parser [Statement]
parseSemicolons (RBrace:rest) = Right ([], rest)
parseSemicolons a = case parseStatement a of
    Right (stmt, Semicolon:rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')

    Right (stmt@(Block _), rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')

    Right (stmt@(If _ _), rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')

    Right (stmt@(While _ _), rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')

    Right (stmt@(Elif _ _), rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')

    Right (stmt@(Else _), rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')



    Right (stuff, thing) -> error ("\n" ++ show stuff ++ "\n\n" ++ show thing)
    a -> error ("THING WITH SEMICOLONS: " ++ show a)


parseStatement :: Parser Statement
parseStatement ((NameT name):EqT:(OperatorT op):rest) = parseStatement ((NameT name):EqT:(NameT name):(OperatorT op):rest)
parseStatement ((NameT name):EqT:rest) = case parseExpr rest of
    Right (thing, rest') -> Right (Assign name thing, rest')
parseStatement (LBrace:rest) = case parseSemicolons rest of
    Right (stmts, rest') -> Right (Block stmts, rest')
parseStatement (KeywordT "If":rest) = case parseExpr rest of
    Right (cond, rest') -> case parseStatement rest' of
        Right (stmt, rest'') -> Right (If cond stmt, rest'')
parseStatement (KeywordT "While":rest) = case parseExpr rest of
    Right (cond, rest') -> case parseStatement rest' of
        Right (stmt, rest'') -> Right (While cond stmt, rest'')
parseStatement (KeywordT "Else":rest) = case parseStatement rest of
    Right (stmt, rest') -> Right (Else stmt, rest')
parseStatement (KeywordT "Elif":rest) = case parseExpr rest of
    Right (cond, rest') -> case parseStatement rest' of
        Right (stmt, rest'') -> Right (Elif cond stmt, rest'')
parseStatement (KeywordT "Var":NameT name:EqT:rest) = case parseExpr rest of
    Right (val, rest') -> Right (DeclAssign name val, rest')
parseStatement (KeywordT "Var":NameT name:rest) = Right (Declare name, rest)
parseStatement (KeywordT "Return":rest) = case parseExpr rest of
    Right (val, rest') -> Right (Return val, rest')
parseStatement (KeywordT "Put":rest) = case parseExpr rest of
    Right (thing1, rest') -> case parseExpr rest' of
        Right (thing2, rest'') -> Right (Put thing1 thing2, rest'')
parseStatement (KeywordT "Shove":rest) = case parseExpr rest of
    Right (thing1, rest') -> case parseExpr rest' of
        Right (thing2, rest'') -> Right (Shove thing1 thing2, rest'')
parseStatement (KeywordT "Kill":rest) = case parseExpr rest of
    Right (thing, rest') -> Right (Kill thing, rest')

parseStatement a = case parseExpr a of
    Right (Get obj field, EqT:rest) -> case parseExpr rest of
        Right (thing, rest') -> case fieldAssign (Get obj field, thing) of
            Left blah -> Right (blah, rest')
            Right blah -> error ("got a weird thing from fieldAssign " ++ show blah)
    Right (val, rest) -> Right (Do val, rest)
    a -> error ("parseStatement: " ++ show a)
   


fieldAssign :: (Expr, Expr) -> Either Statement (Expr, Expr)
fieldAssign (Name blah, rightSide) = Left (Assign blah rightSide)
fieldAssign (Get obj field, rightSide) = fieldAssign (obj, Operator ":" (Parens obj) (List [Str field, rightSide]))
fieldAssign a = Right a

parseFuncDeclArgs :: Parser [String]
parseFuncDeclArgs (RParen:rest) = Right ([], rest)
parseFuncDeclArgs (NameT arg:RParen:rest) = Right ([arg], rest)
parseFuncDeclArgs (NameT arg:Comma:rest) = case parseFuncDeclArgs rest of
    Right (args, rest') -> Right (arg:args, rest')

parseDecl :: Parser Declaration
parseDecl (KeywordT "Def":NameT name:LParen:rest) = case parseFuncDeclArgs rest of
    Right (args, rest') -> case parseStatement rest' of
        Right (body, rest'') -> Right (FuncDec name args body, rest'')
parseDecl (KeywordT "Class":NameT name:LBrace:rest) = case parseDecls rest of
    Right (decls, RBrace:rest') -> Right (ClassDec name decls, rest')
parseDecl (KeywordT "Var":NameT name:rest) = case parseExpr rest of
    Right (val, rest') -> Right (VarDec name val, rest')
parseDecl (KeywordT "Module":NameT modName:LBrace:rest) = case parseProgram rest of
    Right (prgm, RBrace:rest') -> Right (ModDec modName prgm, rest')
parseDecl a = error ("parseDecl screwed up:\n" ++ show a)

parseDecls :: Parser [Declaration]
parseDecls [] = Right ([], [])
parseDecls (RBrace:rest) = Right ([], RBrace:rest)
parseDecls blah = case parseDecl blah of
    Right (decl, rest) -> case parseDecls rest of
        Right (decls, rest') -> Right (decl:decls, rest')

parseProgram :: Parser Program
parseProgram [] = Right (Program [] [], [])
parseProgram (KeywordT "Import":StrT name:rest) = case parseProgram rest of
    Right (Program imports decls, rest') -> Right (Program (name:imports) decls, rest')
parseProgram a = case parseDecls a of
    Right (decls, rest) -> Right (Program [] decls, rest)

expression = parseExpr . tokenize
statement = parseStatement . tokenize
decl = parseDecl . tokenize
program = parseProgram . tokenize