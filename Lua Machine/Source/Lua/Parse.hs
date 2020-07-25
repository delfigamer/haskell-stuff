{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.Parse (
    BinaryOp(..),
    Expression(..),
    ExprNode(..),
    NameNode(..),
    Statement(..),
    StatNode(..),
    UnaryOp(..),
    isValidIdent,
    parseChunk,
) where


import Data.Ratio
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Lua.DefString
import Lua.Lex
import Lua.SourceRange


data NameNode = NameNode (SourceRange, BSt.ByteString) deriving (Show)


instance DefString NameNode where
    defString d (NameNode (_, name)) rest
        = unpackSt name $ rest


newtype VarargNode = VarargNode SourceRange deriving (Show)


instance DefString VarargNode where
    defString d (VarargNode _)
        = "..."


data Expression
    = ExprNil
    | ExprBool Bool
    | ExprInt Integer
    | ExprReal Rational
    | ExprString BSt.ByteString
    | ExprEllipsis
    | ExprFunction
        [NameNode] -- parameter names
        (Maybe VarargNode) -- vararg
        [StatNode] -- body
    | ExprTable [(Maybe ExprNode, ExprNode)] (Maybe ExprNode)
    | ExprVar NameNode
    | ExprIndex ExprNode ExprNode
    | ExprCall ExprNode [ExprNode] (Maybe ExprNode)
    | ExprMethodCall ExprNode BSt.ByteString [ExprNode] (Maybe ExprNode)
    | ExprUnary UnaryOp ExprNode
    | ExprBinary BinaryOp ExprNode ExprNode
    | ExprAnd ExprNode ExprNode
    | ExprOr ExprNode ExprNode
    deriving (Show)


data UnaryOp
    = UnaryNot
    | UnaryLength
    | UnaryMinus
    | UnaryBNot
    deriving (Show)


data BinaryOp
    = BinaryPower
    | BinaryTimes
    | BinaryDivide
    | BinaryFloorDiv
    | BinaryModulo
    | BinaryPlus
    | BinaryMinus
    | BinaryConcat
    | BinaryLShift
    | BinaryRShift
    | BinaryBAnd
    | BinaryBXor
    | BinaryBOr
    | BinaryLess
    | BinaryGreater
    | BinaryLessEqual
    | BinaryGreaterEqual
    | BinaryNotEqual
    | BinaryEqual
    deriving (Show)


instance DefString UnaryOp where
    defString d UnaryNot = "not"
    defString d UnaryLength = "#"
    defString d UnaryMinus = "-"
    defString d UnaryBNot = "~"


instance DefString BinaryOp where
    defString d BinaryPower = "^"
    defString d BinaryTimes = "*"
    defString d BinaryDivide = "/"
    defString d BinaryFloorDiv = "//"
    defString d BinaryModulo = "%"
    defString d BinaryPlus = "+"
    defString d BinaryMinus = "-"
    defString d BinaryConcat = ".."
    defString d BinaryLShift = "<<"
    defString d BinaryRShift = ">>"
    defString d BinaryBAnd = "&"
    defString d BinaryBXor = "~"
    defString d BinaryBOr = "|"
    defString d BinaryLess = "<"
    defString d BinaryGreater = ">"
    defString d BinaryLessEqual = "<="
    defString d BinaryGreaterEqual = ">="
    defString d BinaryNotEqual = "~="
    defString d BinaryEqual = "=="


newtype ExprNode = ExprNode (SourceRange, Expression) deriving (Show)


isMultretExpr :: ExprNode -> Bool
isMultretExpr (ExprNode (_, ExprEllipsis)) = True
isMultretExpr (ExprNode (_, ExprCall _ _ _)) = True
isMultretExpr (ExprNode (_, ExprMethodCall _ _ _ _)) = True
isMultretExpr _ = False


isPrefixExpr :: ExprNode -> Bool
isPrefixExpr (ExprNode (_, ExprVar _)) = True
isPrefixExpr (ExprNode (_, ExprIndex _ _)) = True
isPrefixExpr (ExprNode (_, ExprCall _ _ _)) = True
isPrefixExpr (ExprNode (_, ExprMethodCall _ _ _ _)) = True
isPrefixExpr _ = False


defFunctionHeadless :: Int -> ExprNode -> ShowS
defFunctionHeadless
        d (ExprNode (pr, ExprFunction params vararg body)) rest
    = "(" $ paramStr params vararg $ ")"
        $ defString (d+1) body $ defBr d $ "end" $ rest

    where

    paramStr [] Nothing rest = rest
    paramStr [] (Just va) rest = defString d va $ rest
    paramStr ns Nothing rest = defList ", " d ns $ rest
    paramStr ns (Just va) rest
        = defList ", " d ns $ ", " $ defString d va $ rest


defTableBody :: Int -> [(Maybe ExprNode, ExprNode)] -> ShowS
defTableBody d [] rest = rest
defTableBody d [(Just key, value)] rest
    = defBr d $ "[" $ defString d key $ "] = "
        $ defString d value $ rest
defTableBody d [(Nothing, value)] rest
    = defBr d $ defString d value $ rest
defTableBody d ((Just key, value):x:xs) rest
    = defBr d $ "[" $ defString d key $ "] = "
        $ defString d value $ "," $ defTableBody d (x:xs) $ rest
defTableBody d ((Nothing, value):x:xs) rest
    = defBr d $ defString d value $ "," $ defTableBody d (x:xs) $ rest


defPrefixExpr :: Int -> ExprNode -> ShowS
defPrefixExpr d expr rest
    | isPrefixExpr expr = defString d expr $ rest
    | otherwise = "(" $ defString d expr $ ")" $ rest


instance DefString ExprNode where
    defString d (ExprNode (pr, ExprNil)) rest
        = "nil" $ rest
    defString d (ExprNode (pr, ExprBool False)) rest
        = "false" $ rest
    defString d (ExprNode (pr, ExprBool True)) rest
        = "true" $ rest
    defString d (ExprNode (pr, ExprInt n)) rest
        = shows n $ rest
    defString d (ExprNode (pr, ExprReal n)) rest
        = shows (numerator n) $ "/" $ shows (denominator n) $ rest
    defString d (ExprNode (pr, ExprString s)) rest
        = shows s $ rest
    defString d (ExprNode (pr, ExprEllipsis)) rest
        = "..." $ rest
    defString d e@(ExprNode (pr, ExprFunction _ _ _)) rest
        = "function" $ defFunctionHeadless d e $ rest
    defString d (ExprNode (pr, ExprTable xs Nothing)) rest
        = "{" $ defTableBody (d+1) xs $ "}" $ rest
    defString d (ExprNode (pr, ExprTable xs (Just last))) rest
        = "{" $ defTableBody (d+1) (xs ++ [(Nothing, last)])
            $ " --[[multret]]}" $ rest
    defString d (ExprNode (pr, ExprVar name)) rest
        = defString d name $ rest
    defString d (ExprNode (pr, ExprIndex table key)) rest = do
        case identKey key of
            Just name -> defPrefixExpr d table $ "." $ unpackSt name $ rest
            Nothing -> defPrefixExpr d table
                $ "[" $ defString d key $ "]" $ rest
        where
        identKey (ExprNode (_, ExprString str))
            = if isValidIdent str && not (str `elem` keywords)
                then Just str
                else Nothing
        identKey _ = Nothing
    defString d (ExprNode (pr, ExprCall func args Nothing)) rest
        = defPrefixExpr d func $ "(" $ defList ", " d args $ ")" $ rest
    defString d (ExprNode (pr, ExprCall func args (Just last))) rest
        = defPrefixExpr d func $ "(" $ defList ", " d (args ++ [last])
            $ " --[[multret]])" $ rest
    defString d (ExprNode (pr, ExprMethodCall obj name args Nothing)) rest
        = defPrefixExpr d obj $ ":" $ unpackSt name
            $ "(" $ defList ", " d args $ ")" $ rest
    defString d (ExprNode (pr, ExprMethodCall obj name args (Just last))) rest
        = defPrefixExpr d obj $ ":" $ unpackSt name
            $ "(" $ defList ", " d (args ++ [last]) $ " --[[multret]])" $ rest
    defString d (ExprNode (pr, ExprUnary op x)) rest
        = "(" $ defString d op $ " " $ defString d x $ ")" $ rest
    defString d (ExprNode (pr, ExprBinary op x y)) rest
        = "(" $ defString d x $ " " $ defString d op $ " "
            $ defString d y $ ")" $ rest
    defString d (ExprNode (pr, ExprAnd x y)) rest
        = "(" $ defString d x $ " and "
            $ defString d y $ ")" $ rest
    defString d (ExprNode (pr, ExprOr x y)) rest
        = "(" $ defString d x $ " or "
            $ defString d y $ ")" $ rest


data Statement
    = StatNull
    | StatAssign [ExprNode] [ExprNode] (Maybe ExprNode)
    | StatInvoke ExprNode
    | StatLabel NameNode
    | StatBreak
    | StatGoto NameNode
    | StatDo [StatNode]
    | StatWhile ExprNode [StatNode]
    | StatRepeat [StatNode] ExprNode
    | StatIf ExprNode [StatNode] StatNode
    | StatForNum
        NameNode -- param
        ExprNode -- init
        ExprNode -- final
        (Maybe ExprNode) -- step
        [StatNode] -- body
    | StatForEach [NameNode] [ExprNode] (Maybe ExprNode) [StatNode]
    | StatFunction ExprNode ExprNode
    | StatLocalFunction NameNode ExprNode [StatNode]
    | StatLocalDef
        [(NameNode, Maybe BSt.ByteString)]
        [ExprNode]
        (Maybe ExprNode)
        [StatNode]
    | StatReturn [ExprNode] (Maybe ExprNode)
    deriving (Show)


newtype StatNode = StatNode (SourceRange, Statement) deriving (Show)


defNameAttrs :: Int -> [(NameNode, Maybe BSt.ByteString)] -> ShowS
defNameAttrs d [] rest = rest
defNameAttrs d [(name, Nothing)] rest
    = defString d name $ rest
defNameAttrs d [(name, Just attr)] rest
    = defString d name $ "<" $ unpackSt attr $ ">" $ rest
defNameAttrs d ((name, Nothing):x:xs) rest
    = defString d name $ ", " $ defNameAttrs d (x:xs) $ rest
defNameAttrs d ((name, Just attr):x:xs) rest
    = defString d name $ "<" $ unpackSt attr $ ">, "
        $ defNameAttrs d (x:xs) $ rest


instance DefString StatNode where
    defString d (StatNode (pr, StatNull)) rest
        = ";" $ rest
    defString d (StatNode (pr, StatAssign lhs rhs Nothing)) rest
        = defList ", " d lhs $ " = " $ defList ", " d rhs $ ";" $ rest
    defString d (StatNode (pr, StatAssign lhs rhs (Just last))) rest
        = defList ", " d lhs $ " = " $ defList ", " d (rhs ++ [last])
            $ " --[[multret]];" $ rest
    defString d (StatNode (pr, StatInvoke e)) rest
        = defString d e $ ";" $ rest
    defString d (StatNode (pr, StatLabel name)) rest
        = "::" $ defString d name $ "::;" $ rest
    defString d (StatNode (pr, StatBreak)) rest
        = "break;" $ rest
    defString d (StatNode (pr, StatGoto name)) rest
        = "goto " $ defString d name $ ";" $ rest
    defString d (StatNode (pr, StatDo body)) rest
        = "do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatWhile cond body)) rest
        = "while " $ defString d cond $ " do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatRepeat body cond)) rest
        = "repeat" $ defString (d+1) body
            $ "until " $ defString d cond $ ";" $ rest
    defString d (StatNode (pr, StatIf cond body alt)) rest
        = "if " $ defString d cond $ " then"
            $ defString (d+1) body $ defBr d $ next alt $ rest
        where
        next :: StatNode -> ShowS
        next (StatNode (pr, StatNull)) rest
            = "end;" $ rest
        next (StatNode (pr, StatIf cond body alt)) rest
            = "elseif " $ defString d cond $ " then"
                $ defString (d+1) body $ defBr d $ next alt $ rest
        next (StatNode (pr, StatDo body)) rest
            = "else" $ defString (d+1) body $ defBr d $ "end;" $ rest
        next stat rest
            = "else" $ defString (d+1) [stat] $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForNum pvar a b Nothing body)) rest
        = "for " $ defString d pvar $ " = "
            $ defString d a $ ", " $ defString d b
            $ " do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForNum pvar a b (Just st) body)) rest
        = "for " $ defString d pvar $ " = "
            $ defString d a $ ", " $ defString d b $ ", " $ defString d st
            $ " do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForEach pvars rhs Nothing body)) rest
        = "for " $ defList ", " d pvars  $ " in "
            $ defList ", " d rhs $ " do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatForEach pvars rhs (Just last) body)) rest
        = "for " $ defList ", " d pvars  $ " in "
            $ defList ", " d (rhs ++ [last]) $ " --[[multret]] do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (pr, StatFunction lhs fvalue)) rest
        = "function " $ defString d lhs
            $ defFunctionHeadless d fvalue $ ";" $ rest
    defString d (StatNode (pr, StatLocalFunction name fvalue body)) rest
        = "local function " $ defString d name
            $ defFunctionHeadless d fvalue $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defString d name $ "]]" $ rest
    defString d (StatNode (pr, StatLocalDef nameattrs [] Nothing body)) rest
        = "local " $ defNameAttrs d nameattrs $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr,
            StatLocalDef nameattrs [] (Just last) body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defString d last $ " --[[multret]];"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr, StatLocalDef nameattrs rhs Nothing body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defList ", " d rhs $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr,
            StatLocalDef nameattrs rhs (Just last) body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defList ", " d (rhs ++ [last]) $ " --[[multret]];"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (pr, StatReturn [] Nothing)) rest
        = "return;" $ rest
    defString d (StatNode (pr, StatReturn [] (Just last))) rest
        = "return " $ defString d last $ " --[[multret]];" $ rest
    defString d (StatNode (pr, StatReturn rhs Nothing)) rest
        = "return " $ defList ", " d rhs $ ";" $ rest
    defString d (StatNode (pr, StatReturn rhs (Just last))) rest
        = "return " $ defList ", " d (rhs ++ [last]) $ " --[[multret]];" $ rest


instance DefString [StatNode] where
    defString d [] rest
        = rest
    defString d (x:xs) rest
        = defBr d $ defString d x $ defString d xs $ rest


defChunkString :: [StatNode] -> ShowS
defChunkString stats rest
    = defList "\n" 0 stats $ rest


gramName :: TokParser s NameNode
gramName = do
    (pr, name) <- tokIdent
    return $ NameNode (pr, name)


gramChunk :: TokParser s [StatNode]
gramChunk = do
    body <- gramBlock
    tokEof
    return $ body


gramBlock :: TokParser s [StatNode]
gramBlock = do
    choice [
        do
            stat <- gramReturnStat
            return $ [stat],
        do
            tokSymbol ";"
            gramBlock,
        do
            stat <- gramScopingStat
            rest <- gramBlock
            return $ [stat rest],
        do
            stat <- gramSingleStat
            rest <- gramBlock
            return $ stat:rest,
        return $ []]


gramReturnStat :: TokParser s StatNode
gramReturnStat = do
    pra <- tokKeyword "return"
    (prb, rhs, mlast) <- option (pra, [], Nothing) gramExprList
    prc <- option prb (tokSymbol ";")
    return $ StatNode (pra <> prc, StatReturn rhs mlast)


gramSingleStat :: TokParser s StatNode
gramSingleStat = do
    choice [
        do
            pra <- tokSymbol "::"
            name <- gramName
            prb <- tokSymbol "::"
            return $ StatNode (pra <> prb, StatLabel name),
        do
            pr <- tokKeyword "break"
            return $ StatNode (pr, StatBreak),
        do
            pra <- tokKeyword "goto"
            name@(NameNode (prb, _)) <- gramName
            return $ StatNode (pra <> prb, StatGoto name),
        do
            pra <- tokKeyword "do"
            body <- gramBlock
            prb <- tokKeyword "end"
            return $ StatNode (pra <> prb, StatDo body),
        do
            pra <- tokKeyword "while"
            cond <- gramExpr
            tokKeyword "do"
            body <- gramBlock
            prb <- tokKeyword "end"
            return $ StatNode (pra <> prb, StatWhile cond body),
        do
            pra <- tokKeyword "repeat"
            body <- gramBlock
            tokKeyword "until"
            cond@(ExprNode (prb, _)) <- gramExpr
            return $ StatNode (pra <> prb, StatRepeat body cond),
        gramIfStat,
        gramForStat,
        gramFunctionStat,
        gramExprStat]


gramScopingStat :: TokParser s ([StatNode] -> StatNode)
gramScopingStat = gramLocalFunctionStat <|> gramLocalStat


gramIfStat :: TokParser s StatNode
gramIfStat = do
    pra <- tokKeyword "if"
    cond <- gramExpr
    tokKeyword "then"
    body <- gramBlock
    (prb, alt) <- readAlt
    return $ StatNode (pra <> prb, StatIf cond body alt)

    where

    readAlt :: TokParser s (SourceRange, StatNode)
    readAlt = readElseif <|> readElse <|> readEnd

    readElseif :: TokParser s (SourceRange, StatNode)
    readElseif = do
        pra <- tokKeyword "elseif"
        cond <- gramExpr
        tokKeyword "then"
        body <- gramBlock
        (prb, alt) <- readAlt
        return $ (pra <> prb, StatNode (pra <> prb, StatIf cond body alt))

    readElse :: TokParser s (SourceRange, StatNode)
    readElse = do
        pra <- tokKeyword "else"
        body <- gramBlock
        prb <- tokKeyword "end"
        return $ (pra <> prb, StatNode (pra <> prb, StatDo body))

    readEnd :: TokParser s (SourceRange, StatNode)
    readEnd = do
        pr <- tokKeyword "end"
        return $ (pr, StatNode (pr, StatNull))


gramForStat :: TokParser s StatNode
gramForStat = do
    pra <- tokKeyword "for"
    names <- readNameList
    case names of
        [name] -> gramForNumStat pra name <|> gramForEachStat pra names
        _ -> gramForEachStat pra names

    where

    readNameList :: TokParser s [NameNode]
    readNameList = do
        init <- gramName
        choice [
            do
                tokSymbol ","
                rest <- readNameList
                return $ init:rest,
            return $ [init]]


gramForNumStat :: SourceRange -> NameNode -> TokParser s StatNode
gramForNumStat pra name = do
    tokSymbol "="
    init <- gramExpr
    tokSymbol ","
    final <- gramExpr
    mstep <- optionMaybe (do
        tokSymbol ","
        gramExpr)
    tokKeyword "do"
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ StatNode (pra <> prb, StatForNum name init final mstep body)


gramForEachStat :: SourceRange -> [NameNode] -> TokParser s StatNode
gramForEachStat pra names = do
    tokKeyword "in"
    (_, rhs, mlast) <- gramExprList
    tokKeyword "do"
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ StatNode (pra <> prb, StatForEach names rhs mlast body)


gramFunctionStat :: TokParser s StatNode
gramFunctionStat = do
    try (lookAhead (tokKeyword "function" >> tokIdent))
    pra <- tokKeyword "function"
    (target, isMethod) <- readTarget
    func@(ExprNode (prb, _)) <- gramMethodFunctionBody isMethod
    return $ StatNode (pra <> prb, StatFunction target func)

    where

    readTarget :: TokParser s (ExprNode, Bool)
    readTarget = do
        base <- gramVarExpr
        suffixes base

    suffixes :: ExprNode -> TokParser s (ExprNode, Bool)
    suffixes base
        = suffixIndex base <|> suffixMethod base <|> (return $ (base, False))

    suffixIndex :: ExprNode -> TokParser s (ExprNode, Bool)
    suffixIndex base@(ExprNode (pra, _)) = do
        tokSymbol "."
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixMethod :: ExprNode -> TokParser s (ExprNode, Bool)
    suffixMethod base@(ExprNode (pra, _)) = do
        tokSymbol ":"
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        return $ (ExprNode (pra <> prb, ExprIndex base index), True)


gramLocalFunctionStat :: TokParser s ([StatNode] -> StatNode)
gramLocalFunctionStat = do
    try (lookAhead (tokKeyword "local" >> tokKeyword "function"))
    pra <- tokKeyword "local"
    tokKeyword "function"
    name <- gramName
    func@(ExprNode (prb, _)) <- gramFunctionBody
    return $ (\body ->
        StatNode (pra <> prb, StatLocalFunction name func body))


gramLocalStat :: TokParser s ([StatNode] -> StatNode)
gramLocalStat = do
    pra <- tokKeyword "local"
    nameattrs <- readNameattrs
    localInit pra nameattrs <|> localPure pra nameattrs

    where

    readNameattrs :: TokParser s [(NameNode, Maybe BSt.ByteString)]
    readNameattrs = do
        name <- gramName
        mattr <- optionMaybe readAttr
        choice [
            do
                tokSymbol ","
                rest <- readNameattrs
                return $ (name, mattr):rest,
            return $ [(name, mattr)]]

    readAttr :: TokParser s BSt.ByteString
    readAttr = do
        tokSymbol "<"
        (_, attr) <- tokIdent
        tokSymbol ">"
        return $ attr

    localInit
        :: SourceRange -> [(NameNode, Maybe BSt.ByteString)]
        -> TokParser s ([StatNode] -> StatNode)
    localInit pra nameattrs = do
        tokSymbol "="
        (prb, rhs, mlast) <- gramExprList
        return $ (\body ->
            StatNode (pra <> prb, StatLocalDef nameattrs rhs mlast body))

    localPure
        :: SourceRange -> [(NameNode, Maybe BSt.ByteString)]
        -> TokParser s ([StatNode] -> StatNode)
    localPure pra nameattrs = do
        return $ (\body ->
            StatNode (pra, StatLocalDef nameattrs [] Nothing body))


gramExprStat :: TokParser s StatNode
gramExprStat = do
    (pra, lhs', mlast) <- gramExprList
    let lhs = case mlast of
            Nothing -> lhs'
            Just last -> lhs' ++ [last]
    case lhs of
        [expr] -> assignment pra lhs <|> invocation pra expr
        _ -> assignment pra lhs

    where

    assignment :: SourceRange -> [ExprNode] -> TokParser s StatNode
    assignment pra lhs = do
        tokSymbol "="
        (prb, rhs, mlast) <- gramExprList
        return $ StatNode (pra <> prb, StatAssign lhs rhs mlast)

    invocation :: SourceRange -> ExprNode -> TokParser s StatNode
    invocation pra expr = do
        return $ StatNode (pra, StatInvoke expr)


gramExprList :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
gramExprList = do
    init@(ExprNode(pra, _)) <- gramExpr
    choice [
        do
            tokSymbol ","
            (prb, rest, mlast) <- gramExprList
            return $ (pra <> prb, init:rest, mlast),
        if isMultretExpr init
            then return $ (pra, [], Just init)
            else return $ (pra, [init], Nothing)]


gramExpr :: TokParser s ExprNode
gramExpr = do
    readOr

    where

    readPower :: TokParser s ExprNode
    readPower = do
        base@(ExprNode (pra, _)) <- gramTermExpr
        choice [
            do
                tokSymbol "^"
                exp@(ExprNode (prb, _)) <- readPrefix
                return $
                    ExprNode (pra <> prb, ExprBinary BinaryPower base exp),
            return $ base]

    readPrefix :: TokParser s ExprNode
    readPrefix = do
        choice [
            unaryReader (tokKeyword "not") UnaryNot readPrefix,
            unaryReader (tokSymbol "#") UnaryLength readPrefix,
            unaryReader (tokSymbol "-") UnaryMinus readPrefix,
            unaryReader (tokSymbol "~") UnaryBNot readPrefix,
            readPower]

    readProduct :: TokParser s ExprNode
    readProduct = do
        chainl1 readPrefix $
            choice [
                binaryOp (tokSymbol "*") (ExprBinary BinaryTimes),
                binaryOp (tokSymbol "/") (ExprBinary BinaryDivide),
                binaryOp (tokSymbol "//") (ExprBinary BinaryFloorDiv),
                binaryOp (tokSymbol "%") (ExprBinary BinaryModulo)]

    readSum :: TokParser s ExprNode
    readSum = do
        chainl1 readProduct $
            choice [
                binaryOp (tokSymbol "+") (ExprBinary BinaryPlus),
                binaryOp (tokSymbol "-") (ExprBinary BinaryMinus)]

    readJoin :: TokParser s ExprNode
    readJoin = do
        chainr1 readSum $
            binaryOp (tokSymbol "..") (ExprBinary BinaryConcat)

    readShift :: TokParser s ExprNode
    readShift = do
        chainr1 readJoin $
            binaryOp (tokSymbol "..") (ExprBinary BinaryConcat)

    readBitAnd :: TokParser s ExprNode
    readBitAnd = do
        chainl1 readShift $
            binaryOp (tokSymbol "&") (ExprBinary BinaryBAnd)

    readBitXor :: TokParser s ExprNode
    readBitXor = do
        chainl1 readBitAnd $
            binaryOp (tokSymbol "~") (ExprBinary BinaryBXor)

    readBitOr :: TokParser s ExprNode
    readBitOr = do
        chainl1 readBitXor $
            binaryOp (tokSymbol "|") (ExprBinary BinaryBOr)

    readCompare :: TokParser s ExprNode
    readCompare = do
        chainl1 readBitOr $
            choice [
                binaryOp (tokSymbol "<") (ExprBinary BinaryLess),
                binaryOp (tokSymbol ">") (ExprBinary BinaryGreater),
                binaryOp (tokSymbol "<=") (ExprBinary BinaryLessEqual),
                binaryOp (tokSymbol ">=") (ExprBinary BinaryGreaterEqual),
                binaryOp (tokSymbol "~=") (ExprBinary BinaryNotEqual),
                binaryOp (tokSymbol "==") (ExprBinary BinaryEqual)]

    readAnd :: TokParser s ExprNode
    readAnd = do
        chainl1 readCompare $
            binaryOp (tokKeyword "and") ExprAnd

    readOr :: TokParser s ExprNode
    readOr = do
        chainl1 readAnd $
            binaryOp (tokKeyword "or") ExprOr

    unaryReader
        :: TokParser s SourceRange
        -> UnaryOp
        -> TokParser s ExprNode
        -> TokParser s ExprNode
    unaryReader prefixParser op elemParser = do
        pra <- prefixParser
        arg@(ExprNode (prb, _)) <- elemParser
        return $ ExprNode (pra <> prb, ExprUnary op arg)

    binaryOp
        :: TokParser s SourceRange
        -> (ExprNode -> ExprNode -> Expression)
        -> TokParser s (ExprNode -> ExprNode -> ExprNode)
    binaryOp parser builder = do
        parser
        return $ (\left@(ExprNode (pra, _)) right@(ExprNode (prb, _)) ->
            ExprNode (pra <> prb, builder left right))


gramTermExpr :: TokParser s ExprNode
gramTermExpr = do
    choice [
        atom "nil" ExprNil,
        atom "false" (ExprBool False),
        atom "true" (ExprBool True),
        literal tokInt ExprInt,
        literal tokReal ExprReal,
        literal tokString ExprString,
        do
            pr <- tokSymbol "..."
            return $ ExprNode (pr, ExprEllipsis),
        do
            pr <- tokKeyword "function"
            gramFunctionBody,
        gramTableExpr,
        gramSuffixExpr]

    where

    atom :: BSt.ByteString -> Expression -> TokParser s ExprNode
    atom kw nd = do
        pr <- tokKeyword kw
        return $ ExprNode (pr, nd)

    literal
        :: TokParser s (SourceRange, a)
        -> (a -> Expression)
        -> TokParser s ExprNode
    literal parse construct = do
        (pr, v) <- parse
        return $ ExprNode (pr, construct v)


gramFunctionBody :: TokParser s ExprNode
gramFunctionBody = do
    gramMethodFunctionBody False


gramMethodFunctionBody :: Bool -> TokParser s ExprNode
gramMethodFunctionBody isMethod = do
    pra <- tokSymbol "("
    (params, vararg) <- gramParamList
    tokSymbol ")"
    let params' =
            if isMethod
                then (NameNode (pra, "self")):params
                else params
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ ExprNode (pra <> prb, ExprFunction params' vararg body)


gramParamList :: TokParser s ([NameNode], Maybe VarargNode)
gramParamList = do
    option ([], Nothing) readParams

    where

    readParams :: TokParser s ([NameNode], Maybe VarargNode)
    readParams = paramVararg <|> paramName

    paramVararg :: TokParser s ([NameNode], Maybe VarargNode)
    paramVararg = do
        pr <- tokSymbol "..."
        return $ ([], Just (VarargNode pr))

    paramName :: TokParser s ([NameNode], Maybe VarargNode)
    paramName = do
        name <- gramName
        choice [
            do
                tokSymbol ","
                (rest, vararg) <- readParams
                return $ (name:rest, vararg),
            return $ ([name], Nothing)]


gramTableExpr :: TokParser s ExprNode
gramTableExpr = do
    pra <- tokSymbol "{"
    (elems, mlast) <- readFields
    prb <- tokSymbol "}"
    return $ ExprNode (pra <> prb, ExprTable elems mlast)

    where

    readFields :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    readFields = do
        choice [
            fieldIndexed,
            fieldNamed,
            fieldPos,
            return $ ([], Nothing)]

    fieldIndexed :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    fieldIndexed = do
        tokSymbol "["
        key <- gramExpr
        tokSymbol "]"
        tokSymbol "="
        value <- gramExpr
        (rest, mlast) <- readTableRest
        return $ ((Just key, value):rest, mlast)

    fieldNamed :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    fieldNamed = do
        try (lookAhead (do
            tokIdent
            tokSymbol "="))
        (pr, nameid) <- tokIdent
        let key = ExprNode (pr, ExprString nameid)
        tokSymbol "="
        value <- gramExpr
        (rest, mlast) <- readTableRest
        return $ ((Just key, value):rest, mlast)

    fieldPos :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    fieldPos = do
        value <- gramExpr
        (rest, mlast) <- readTableRest
        case (rest, mlast) of
            ([], Nothing) ->
                if isMultretExpr value
                    then return $ ([], Just value)
                    else return $ ((Nothing, value):rest, mlast)
            _ -> return $ ((Nothing, value):rest, mlast)

    readTableRest :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    readTableRest = do
        choice [
            do
                tokSymbol "," <|> tokSymbol ";"
                readFields,
            return $ ([], Nothing)]


gramVarExpr :: TokParser s ExprNode
gramVarExpr = do
    name@(NameNode (pr, _)) <- gramName
    return $ ExprNode (pr, ExprVar name)


gramSuffixExpr :: TokParser s ExprNode
gramSuffixExpr = do
    start <- readGroup <|> gramVarExpr
    suffixes $ start

    where

    readGroup :: TokParser s ExprNode
    readGroup = do
        pra <- tokSymbol "("
        expr <- gramExpr
        prb <- tokSymbol ")"
        return $ expr

    suffixes :: ExprNode -> TokParser s ExprNode
    suffixes base = do
        choice [
            suffixExprIndex base,
            suffixNameIndex base,
            suffixMethod base,
            suffixApply base,
            return $ base]

    suffixExprIndex :: ExprNode -> TokParser s ExprNode
    suffixExprIndex base@(ExprNode (pra, _)) = do
        tokSymbol "["
        index <- gramExpr
        prb <- tokSymbol "]"
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixNameIndex :: ExprNode -> TokParser s ExprNode
    suffixNameIndex base@(ExprNode (pra, _)) = do
        tokSymbol "."
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixMethod :: ExprNode -> TokParser s ExprNode
    suffixMethod base@(ExprNode (pra, _)) = do
        tokSymbol ":"
        (_, name) <- tokIdent
        (prb, args, mlast) <- arguments
        let nstr = name
        suffixes $ ExprNode (pra <> prb, ExprMethodCall base nstr args mlast)

    suffixApply :: ExprNode -> TokParser s ExprNode
    suffixApply base@(ExprNode (pra, _)) = do
        (prb, args, mlast) <- arguments
        suffixes $ ExprNode (pra <> prb, ExprCall base args mlast)

    arguments :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    arguments = argList <|> argString <|> argTable

    argList :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    argList = do
        pra <- tokSymbol "("
        (args, mlast) <- option ([], Nothing) argListRest
        prb <- tokSymbol ")"
        return $ (pra <> prb, args, mlast)

    argListRest :: TokParser s ([ExprNode], Maybe ExprNode)
    argListRest = do
        head <- gramExpr
        choice [
            do
                tokSymbol ","
                (tail, mlast) <- argListRest
                return $ ((head:tail), mlast),
            if isMultretExpr head
                then return $ ([], Just head)
                else return $ ([head], Nothing)]

    argString :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    argString = do
        (pr, str) <- tokString
        return $ (pr, [ExprNode (pr, ExprString str)], Nothing)

    argTable :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
    argTable = do
        arg@(ExprNode (pr, _)) <- gramTableExpr
        return $ (pr, [arg], Nothing)


parseChunk
    :: SourceName
    -> B.ByteString
    -> Either ParseError [StatNode]
parseChunk = parseGrammar gramChunk
