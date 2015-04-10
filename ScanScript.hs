module ScanScript where
import ScanLib




data Token = OperatorT String
    | NumberT Int
    | StrT String
    | NameT String
    | LBracket
    | RBracket
    | LBrace
    | RBrace
    | LParen
    | RParen
    | Comma
    | Semicolon 
    | KeywordT String
    | EqT deriving (Show, Eq)


readOperator :: Reader Token
readOperator a = OperatorT a

readNumber :: Reader Token
readNumber a = NumberT (read a)

readString :: Reader Token
readString a = StrT (read a)

readName :: Reader Token
readName a = NameT a

readSymbol :: Reader Token
readSymbol "(" = LParen
readSymbol ")" = RParen
readSymbol "{" = LBrace
readSymbol "}" = RBrace
readSymbol "[" = LBracket
readSymbol "]" = RBracket
readSymbol "," = Comma
readSymbol ";" = Semicolon

readKeyword :: Reader Token
readKeyword a = KeywordT a

isSymbol a = elem a "(){}[],;"

isOperator a = elem a "<>+:/$#!&*%^|-=?."
isOperatorStart a = elem a "<>+:/$#!&*%^|-?."

isDigit a = elem a "1234567890"

isNameStart a = elem a "_abcdefghijklmnopqrstuvwxyz"
isNameBody a = (isNameStart a) || (elem a "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")

isKeywordStart a = elem a "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
isKeywordBody a = (elem a "abcdefghijklmnopqrstuvwxyz") || (isKeywordStart a)

isSpace a = elem a " \r\n\t"

scriptClauses = [
    Clause 0 (== '"') append 1,
    Clause 1 (== '\\') append 7,
    Clause 1 (/= '"') append 1,
    Clause 7 (const True) append 1,
    Clause 1 (== '"') (appendEmit readString) 0,
    Clause 0 isSymbol (appendEmit readSymbol) 0,
    Clause 0 isOperatorStart append 2,
    Clause 2 isOperator append 2,
    Clause 2 (not . isOperator) (emitPush readOperator) 0,
    Clause 0 isDigit append 3,
    Clause 0 (== '-') append 8,
    Clause 8 isDigit append 3,
    Clause 8 isOperator append 2,
    Clause 8 (not . isDigit) (emitPush readOperator) 0,
    Clause 3 isDigit append 3,
    Clause 3 (not . isDigit) (emitPush readNumber) 0,
    Clause 0 isSpace ignore 0,
    Clause 0 (== '@') ignore 4,
    Clause 4 (== '\n') ignore 0,
    Clause 4 (const True) ignore 4,
    Clause 0 isNameStart append 5,
    Clause 5 isNameBody append 5,
    Clause 5 (not . isNameBody) (emitPush readName) 0,
    Clause 0 isKeywordStart append 6,
    Clause 6 isKeywordBody append 6,
    Clause 6 (not . isKeywordBody) (emitPush readKeyword) 0,
    Clause 0 (== '=') (emit (const EqT)) 0]



tokenize :: String -> [Token]
tokenize text = case clausesDoStr scriptClauses (text ++ " ") of
    (_, (_, _, tokens)) -> tokens

blah = clausesDoStr scriptClauses

isStringOperator :: String -> Bool
isStringOperator (thing:stuff) = (isOperatorStart thing) && all isOperator stuff