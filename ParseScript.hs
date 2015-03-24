module ParseScript where

import ScanScript


data Statement = Assign String Expr
    | Do Expr
    | Block [Statement]
    | While Expr Statement
    | If Expr Statement
    | Else Statement
    | Elif Expr Statement
    | Declare String
    | DeclAssign String Expr
    | Return Expr deriving (Show, Eq)



data Expr = Number Int
    | Str String
    | Parens Expr
    | Operator String Expr Expr
    | Call String [Expr]
    | List [Expr]
    | Name String 
    | Get Expr String
    | Method Expr String [Expr] deriving (Show, Eq)


data Declaration = FuncDec String [String] Statement
    | ClassDec String [Declaration] deriving (Show)



data Program = Program [Declaration] deriving (Show)


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

parseOp (Right (a, (OperatorT ".":NameT field:LParen:rest))) = parseOp $ case parseCommaStuff rest of
    Right (args, (RParen:rest')) -> Right (Method a field args, rest')

parseOp (Right (a, (OperatorT ".":NameT field:rest))) = parseOp $ Right (Get a field, rest)



parseOp (Right (arg1, (OperatorT op:rest))) = case parseExpr rest of
    Right (arg2, rest') -> Right (Operator op arg1 arg2, rest')
parseOp a = a


parseSemicolons :: Parser [Statement]
parseSemicolons (RBrace:rest) = Right ([], rest)
parseSemicolons a = case parseStatement a of
    Right (stmt, Semicolon:rest) -> case parseSemicolons rest of
        Right (stmts, rest') -> Right (stmt:stmts, rest')
    a -> error (show a)


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
parseStatement a = case parseExpr a of
    Right (val, rest) -> Right (Do val, rest)


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
    Right (decls, rest') -> Right (ClassDec name decls, rest')
parseDecl a = error (show a)

parseDecls :: Parser [Declaration]
parseDecls (RBrace:rest) = Right ([], rest)
parseDecls blah = case parseDecl blah of
    Right (decl, rest) -> case parseDecls rest of
        Right (decls, rest') -> Right (decl:decls, rest')

parseProgram :: Parser Program
parseProgram [] = Right (Program [], [])
parseProgram a = case parseDecl a of
    Right (def, rest) -> case parseProgram rest of
        Right (Program defs, rest') -> Right (Program (def:defs), rest')

expression = parseExpr . tokenize
statement = parseStatement . tokenize
decl = parseDecl . tokenize
program = parseProgram . tokenize