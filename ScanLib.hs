



module ScanLib where





data Clause a = Clause Int (Char -> Bool) (Action a) Int


type Status a = (Maybe Char, String, [a])


type Action a = Status a -> Char -> Status a

type Reader a = String -> a

append :: Action a
append (Nothing, buff, tokens) char = (Nothing, buff ++ [char], tokens)

appendEmit :: Reader a -> Action a
appendEmit reader (Nothing, buff, tokens) char = (Nothing, [], tokens ++ [reader (buff ++ [char])])

emitPush :: Reader a -> Action a
emitPush reader (Nothing, buff, tokens) char = (Just char, [], tokens ++ [reader buff])

emit reader (Nothing, buff, tokens) char = (Nothing, [], tokens ++ [reader buff])


ignore :: Action a
ignore a char = a


clauseDo :: Clause a -> Char -> (Int, Status a) -> (Int, Status a)
clauseDo c@(Clause cond predicate action result) char (state, status)
    | matchesClause (state, char) c = (result, action status char)
    | otherwise = (state, status)

matchesClause :: (Int, Char) -> Clause a -> Bool
matchesClause (state, char) (Clause cond predicate _ _) = (state == cond) && predicate char

clausesDo :: [Clause a] -> Char -> (Int, Status a) -> (Int, Status a)
clausesDo [] _ thing = thing
clausesDo clauses char (state, (Just char', buff, tokens)) = clausesDo clauses char (clausesDo clauses char' (0, (Nothing, buff, tokens)))
clausesDo (clause:clauses) char (state, status) | matchesClause (state, char) clause = clauseDo clause char (state, status)
    | otherwise = clausesDo clauses char (state, status)




clausesDoStr' :: [Clause a] -> String -> (Int, Status a) -> (Int, Status a)
clausesDoStr' clauses "" thing = thing
clausesDoStr' clauses (char:chars) thing = clausesDoStr' clauses chars (clausesDo clauses char thing)

clausesDoStr :: [Clause a] -> String -> (Int, Status a)
clausesDoStr clauses str = clausesDoStr' clauses str (0, (Nothing, [], []))



