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



unStatement :: Statement -> [Token]
unStatement = undefined