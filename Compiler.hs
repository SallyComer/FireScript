module Compiler where
import ParseScript
import ASTData
import ScanScript


unCommaStuff :: [Expr] -> [Token]
unCommaStuff [] = []
unCommaStuff [a] = unExpression a
unCommaStuff (x:xs) = (unExpression x) ++ [Comma] ++ unCommaStuff xs

unExpression :: Expr -> [Token]
unExpression (Lambda args body) = KeywordT "Lambda" : unExpression (Tuple $ map Name args) ++ unStatement body
unExpression Ignite = [KeywordT "Ignite"]
unExpression (Spark thing) = KeywordT "Spark" : unExpression thing
unExpression (Take thing) = KeywordT "Take" : unExpression thing
unExpression (Read thing) = KeywordT "Read" : unExpression thing
unExpression (Tuple args) = LParen : unCommaStuff args ++ [RParen]
unExpression (Number a) = [NumberT a]
unExpression (Name a) = [NameT a]
unExpression (Str a) = [StrT a]
unExpression (List a) = LBracket : unCommaStuff a ++ [RBracket]
unExpression (Operator op arg1 arg2) = (unExpression arg1) ++ [OperatorT op] ++ (unExpression arg2)
unExpression (Call func args) = unExpression func ++ (unExpression $ Tuple args)
unExpression (Method obj name args) = unExpression obj ++ [OperatorT ".", NameT name] ++ (unExpression $ Tuple args)
unExpression (MemberAccess namespace name) = unExpression namespace ++ [OperatorT "::", NameT name]
unExpression (Get obj name) = unExpression obj ++ [OperatorT ".", NameT name]
unExpression (Parens expr) = LParen : unExpression expr ++ [RParen]
unExpression a = error $ "unExpression: " ++ show a



unStatement :: Statement -> [Token]
unStatement (Assign name thing) = NameT name : EqT : unExpression thing ++ [Semicolon]
unStatement (Block stmts) = LBrace : unStatements stmts
unStatement (DeclAssign name val) = KeywordT "Var" : NameT name : EqT : unExpression val ++ [Semicolon]
unStatement (Declare name) = KeywordT "Var" : NameT name : [Semicolon]
unStatement (Do expr) = unExpression expr ++ [Semicolon]
unStatement (Command name stuff) = KeywordT name : unThings stuff ++ [Semicolon]


unStatements :: [Statement] -> [Token]
unStatements stmts = (stmts >>= unStatement) ++ [RBrace]


unThing :: UserMade -> [Token]
unThing (UserString str) = [StrT str]
unThing (UserIdentifier name) = [NameT name]
unThing (UserValue expr) = unExpression expr
unThing (UserCommand stmt) = unStatement stmt
unThing (UserKeyword word) = [KeywordT word]
unThing (UserArgs args) = unExpression $ Tuple args
unThing (UserToken tok) = [tok]


unThings :: [UserMade] -> [Token]
unThings stuff = stuff >>= unThing


unDecl :: Declaration -> [Token]
unDecl (FuncDec name args body) | isStringOperator name = case args of
        [arg1, arg2] -> KeywordT "Operator" : NameT arg1 : OperatorT name : NameT arg2 : unStatement body
        a -> error "wrong amount of args in operator declaration"
    | otherwise = KeywordT name : (unExpression $ Tuple $ map Name args) ++ unStatement body
unDecl (ClassDec name decls) = KeywordT "Class" : NameT name : LBrace : unDecls decls ++ [RBrace]
unDecl (VarDec name val) = KeywordT "Var" : NameT name : unExpression val ++ [Semicolon]
unDecl (ModDec name prgm) = KeywordT "Module" : NameT name : LBrace : unProgram prgm ++ [RBrace]

unDecls :: [Declaration] -> [Token]
unDecls stuff = (stuff >>= unDecl)

unProgram :: Program -> [Token]
unProgram (Program prgrm) = unDecls prgrm



unToken :: Token -> String
unToken (OperatorT op) = op
unToken (NumberT i) = show i
unToken (NameT name) = name
unToken (StrT str) = show str
unToken (KeywordT word) = word
unToken LBracket = "["
unToken RBracket = "]"
unToken LBrace = "{"
unToken RBrace = "}"
unToken LParen = "("
unToken RParen = ")"
unToken Comma = ","
unToken Semicolon = ";\n"
unToken EqT = "="



unTokens :: [Token] -> String
unTokens toks = unwords (fmap unToken toks)