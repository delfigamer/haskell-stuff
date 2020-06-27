module Parse (
) where

import qualified Data.ByteString.Char8 as BSt
import Text.Parsec
import Lex
import Expr

-- gramTest :: TokParser [String]
-- gramTest = do
    -- words <- many tokWord
    -- tokEof
    -- return $ map (\t@(_, buf) -> t) words

gramDatum :: TokParser s SExpr
gramDatum = choice [
    do
        (pr, v) <- tokBool
        return $ SExpr (pr, SBool v),
    do
        (pr, v) <- tokNumber
        return $ SExpr (pr, SNumber v),
    do
        (pr, v) <- tokChar
        return $ SExpr (pr, SChar v),
    do
        (pr, v) <- tokString
        return $ SExpr (pr, SString v),
    do
        (pr, v) <- tokWord
        return $ SExpr (pr, SAtom v)]
