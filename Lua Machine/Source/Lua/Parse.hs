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
    defString _ (NameNode (_, name)) rest
        = unpackSt name $ rest


newtype VarargNode = VarargNode SourceRange deriving (Show)


instance DefString VarargNode where
    defString _ (VarargNode _)
        = "..."


data Expression
    = ExprNil
    | ExprBool Bool
    | ExprInt Integer
    | ExprReal Double
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
    | ExprGroup ExprNode
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
    defString _ UnaryNot = "not"
    defString _ UnaryLength = "#"
    defString _ UnaryMinus = "-"
    defString _ UnaryBNot = "~"


instance DefString BinaryOp where
    defString _ BinaryPower = "^"
    defString _ BinaryTimes = "*"
    defString _ BinaryDivide = "/"
    defString _ BinaryFloorDiv = "//"
    defString _ BinaryModulo = "%"
    defString _ BinaryPlus = "+"
    defString _ BinaryMinus = "-"
    defString _ BinaryConcat = ".."
    defString _ BinaryLShift = "<<"
    defString _ BinaryRShift = ">>"
    defString _ BinaryBAnd = "&"
    defString _ BinaryBXor = "~"
    defString _ BinaryBOr = "|"
    defString _ BinaryLess = "<"
    defString _ BinaryGreater = ">"
    defString _ BinaryLessEqual = "<="
    defString _ BinaryGreaterEqual = ">="
    defString _ BinaryNotEqual = "~="
    defString _ BinaryEqual = "=="


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
        d (ExprNode (_, ExprFunction params vararg body)) rest
    = "(" $ paramStr params vararg $ ")"
        $ defString (d+1) body $ defBr d $ "end" $ rest

    where

    paramStr [] Nothing rest' = rest'
    paramStr [] (Just va) rest' = defString d va $ rest'
    paramStr ns Nothing rest' = defList ", " d ns $ rest'
    paramStr ns (Just va) rest'
        = defList ", " d ns $ ", " $ defString d va $ rest'


defFunctionHeadless _ _ _ = undefined


defTableBody :: Int -> [(Maybe ExprNode, ExprNode)] -> ShowS
defTableBody _ [] rest = rest
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
    defString _ (ExprNode (_, ExprNil)) rest
        = "nil" $ rest
    defString _ (ExprNode (_, ExprBool False)) rest
        = "false" $ rest
    defString _ (ExprNode (_, ExprBool True)) rest
        = "true" $ rest
    defString _ (ExprNode (_, ExprInt n)) rest
        = shows n $ rest
    defString _ (ExprNode (_, ExprReal n)) rest
        = shows n $ rest
    defString _ (ExprNode (_, ExprString s)) rest
        = shows s $ rest
    defString _ (ExprNode (_, ExprEllipsis)) rest
        = "..." $ rest
    defString d e@(ExprNode (_, ExprFunction _ _ _)) rest
        = "function" $ defFunctionHeadless d e $ rest
    defString d (ExprNode (_, ExprTable xs Nothing)) rest
        = "{" $ defTableBody (d+1) xs $ "}" $ rest
    defString d (ExprNode (_, ExprTable xs (Just elast))) rest
        = "{" $ defTableBody (d+1) (xs ++ [(Nothing, elast)])
            $ " --[[multret]]}" $ rest
    defString d (ExprNode (_, ExprVar name)) rest
        = defString d name $ rest
    defString d (ExprNode (_, ExprIndex table key)) rest = do
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
    defString d (ExprNode (_, ExprCall func args Nothing)) rest
        = defPrefixExpr d func $ "(" $ defList ", " d args $ ")" $ rest
    defString d (ExprNode (_, ExprCall func args (Just elast))) rest
        = defPrefixExpr d func $ "(" $ defList ", " d (args ++ [elast])
            $ " --[[multret]])" $ rest
    defString d (ExprNode (_, ExprMethodCall obj name args Nothing)) rest
        = defPrefixExpr d obj $ ":" $ unpackSt name
            $ "(" $ defList ", " d args $ ")" $ rest
    defString d (ExprNode (_, ExprMethodCall obj name args (Just elast))) rest
        = defPrefixExpr d obj $ ":" $ unpackSt name
            $ "(" $ defList ", " d (args ++ [elast]) $ " --[[multret]])" $ rest
    defString d (ExprNode (_, ExprUnary op x)) rest
        = "(" $ defString d op $ " " $ defString d x $ ")" $ rest
    defString d (ExprNode (_, ExprBinary op x y)) rest
        = "(" $ defString d x $ " " $ defString d op $ " "
            $ defString d y $ ")" $ rest
    defString d (ExprNode (_, ExprAnd x y)) rest
        = "(" $ defString d x $ " and "
            $ defString d y $ ")" $ rest
    defString d (ExprNode (_, ExprOr x y)) rest
        = "(" $ defString d x $ " or "
            $ defString d y $ ")" $ rest
    defString d (ExprNode (_, ExprGroup x)) rest
        = "(" $ defString d x $ ")" $ rest


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
defNameAttrs _ [] rest = rest
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
    defString _ (StatNode (_, StatNull)) rest
        = ";" $ rest
    defString d (StatNode (_, StatAssign lhs rhs Nothing)) rest
        = defList ", " d lhs $ " = " $ defList ", " d rhs $ ";" $ rest
    defString d (StatNode (_, StatAssign lhs rhs (Just elast))) rest
        = defList ", " d lhs $ " = " $ defList ", " d (rhs ++ [elast])
            $ " --[[multret]];" $ rest
    defString d (StatNode (_, StatInvoke e)) rest
        = defString d e $ ";" $ rest
    defString d (StatNode (_, StatLabel name)) rest
        = "::" $ defString d name $ "::;" $ rest
    defString _ (StatNode (_, StatBreak)) rest
        = "break;" $ rest
    defString d (StatNode (_, StatGoto name)) rest
        = "goto " $ defString d name $ ";" $ rest
    defString d (StatNode (_, StatDo body)) rest
        = "do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (_, StatWhile cond body)) rest
        = "while " $ defString d cond $ " do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (_, StatRepeat body cond)) rest
        = "repeat" $ defString (d+1) body
            $ "until " $ defString d cond $ ";" $ rest
    defString d (StatNode (_, StatIf cond body alt)) rest
        = "if " $ defString d cond $ " then"
            $ defString (d+1) body $ defBr d $ next alt $ rest
        where
        next :: StatNode -> ShowS
        next (StatNode (_, StatNull)) rest'
            = "end;" $ rest'
        next (StatNode (_, StatIf cond' body' alt')) rest'
            = "elseif " $ defString d cond' $ " then"
                $ defString (d+1) body' $ defBr d $ next alt' $ rest'
        next (StatNode (_, StatDo body')) rest'
            = "else" $ defString (d+1) body' $ defBr d $ "end;" $ rest'
        next stat rest'
            = "else" $ defString (d+1) [stat] $ defBr d $ "end;" $ rest'
    defString d (StatNode (_, StatForNum pvar a b Nothing body)) rest
        = "for " $ defString d pvar $ " = "
            $ defString d a $ ", " $ defString d b
            $ " do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (_, StatForNum pvar a b (Just st) body)) rest
        = "for " $ defString d pvar $ " = "
            $ defString d a $ ", " $ defString d b $ ", " $ defString d st
            $ " do" $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (_, StatForEach pvars rhs Nothing body)) rest
        = "for " $ defList ", " d pvars  $ " in "
            $ defList ", " d rhs $ " do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (_, StatForEach pvars rhs (Just elast) body)) rest
        = "for " $ defList ", " d pvars  $ " in "
            $ defList ", " d (rhs ++ [elast]) $ " --[[multret]] do"
            $ defString (d+1) body $ defBr d $ "end;" $ rest
    defString d (StatNode (_, StatFunction lhs fvalue)) rest
        = "function " $ defString d lhs
            $ defFunctionHeadless d fvalue $ ";" $ rest
    defString d (StatNode (_, StatLocalFunction name fvalue body)) rest
        = "local function " $ defString d name
            $ defFunctionHeadless d fvalue $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defString d name $ "]]" $ rest
    defString d (StatNode (_, StatLocalDef nameattrs [] Nothing body)) rest
        = "local " $ defNameAttrs d nameattrs $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (_,
            StatLocalDef nameattrs [] (Just elast) body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defString d elast $ " --[[multret]];"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (_, StatLocalDef nameattrs rhs Nothing body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defList ", " d rhs $ ";"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString d (StatNode (_,
            StatLocalDef nameattrs rhs (Just elast) body)) rest
        = "local " $ defNameAttrs d nameattrs $ " = "
            $ defList ", " d (rhs ++ [elast]) $ " --[[multret]];"
            $ defString d body $ defBr d
            $ "--[[unlocal " $ defNameAttrs d nameattrs $ "]]" $ rest
    defString _ (StatNode (_, StatReturn [] Nothing)) rest
        = "return;" $ rest
    defString d (StatNode (_, StatReturn [] (Just elast))) rest
        = "return " $ defString d elast $ " --[[multret]];" $ rest
    defString d (StatNode (_, StatReturn rhs Nothing)) rest
        = "return " $ defList ", " d rhs $ ";" $ rest
    defString d (StatNode (_, StatReturn rhs (Just elast))) rest
        = "return " $ defList ", " d (rhs ++ [elast]) $ " --[[multret]];" $ rest


instance DefString [StatNode] where
    defString _ [] rest
        = rest
    defString d (x:xs) rest
        = defBr d $ defString d x $ defString d xs $ rest


gramName :: TokParser s NameNode
gramName = do
    (pr, name) <- tokIdent
    return $ NameNode (pr, name)


gramChunk :: TokParser s [StatNode]
gramChunk = do
    body <- gramBlock
    _ <- tokEof
    return $ body


gramBlock :: TokParser s [StatNode]
gramBlock = do
    choice [
        do
            stat <- gramReturnStat
            return $ [stat],
        do
            _ <- tokSymbol ";"
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
            _ <- tokKeyword "do"
            body <- gramBlock
            prb <- tokKeyword "end"
            return $ StatNode (pra <> prb, StatWhile cond body),
        do
            pra <- tokKeyword "repeat"
            body <- gramBlock
            _ <- tokKeyword "until"
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
    _ <- tokKeyword "then"
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
        _ <- tokKeyword "then"
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
        tinit <- gramName
        choice [
            do
                _ <- tokSymbol ","
                rest <- readNameList
                return $ tinit:rest,
            return $ [tinit]]


gramForNumStat :: SourceRange -> NameNode -> TokParser s StatNode
gramForNumStat pra name = do
    _ <- tokSymbol "="
    tinit <- gramExpr
    _ <- tokSymbol ","
    tfinal <- gramExpr
    mstep <- optionMaybe (do
        _ <- tokSymbol ","
        gramExpr)
    _ <- tokKeyword "do"
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ StatNode (pra <> prb, StatForNum name tinit tfinal mstep body)


gramForEachStat :: SourceRange -> [NameNode] -> TokParser s StatNode
gramForEachStat pra names = do
    _ <- tokKeyword "in"
    (_, rhs, mlast) <- gramExprList
    _ <- tokKeyword "do"
    body <- gramBlock
    prb <- tokKeyword "end"
    return $ StatNode (pra <> prb, StatForEach names rhs mlast body)


gramFunctionStat :: TokParser s StatNode
gramFunctionStat = do
    _ <- try (lookAhead (tokKeyword "function" >> tokIdent))
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
        _ <- tokSymbol "."
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixMethod :: ExprNode -> TokParser s (ExprNode, Bool)
    suffixMethod base@(ExprNode (pra, _)) = do
        _ <- tokSymbol ":"
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        return $ (ExprNode (pra <> prb, ExprIndex base index), True)


gramLocalFunctionStat :: TokParser s ([StatNode] -> StatNode)
gramLocalFunctionStat = do
    _ <- try (lookAhead (tokKeyword "local" >> tokKeyword "function"))
    pra <- tokKeyword "local"
    _ <- tokKeyword "function"
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
                _ <- tokSymbol ","
                rest <- readNameattrs
                return $ (name, mattr):rest,
            return $ [(name, mattr)]]

    readAttr :: TokParser s BSt.ByteString
    readAttr = do
        _ <- tokSymbol "<"
        (_, attr) <- tokIdent
        _ <- tokSymbol ">"
        return $ attr

    localInit
        :: SourceRange -> [(NameNode, Maybe BSt.ByteString)]
        -> TokParser s ([StatNode] -> StatNode)
    localInit pra nameattrs = do
        _ <- tokSymbol "="
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
            Just tlast -> lhs' ++ [tlast]
    case lhs of
        [expr] -> assignment pra lhs <|> invocation pra expr
        _ -> assignment pra lhs

    where

    assignment :: SourceRange -> [ExprNode] -> TokParser s StatNode
    assignment pra lhs = do
        _ <- tokSymbol "="
        (prb, rhs, mlast) <- gramExprList
        return $ StatNode (pra <> prb, StatAssign lhs rhs mlast)

    invocation :: SourceRange -> ExprNode -> TokParser s StatNode
    invocation pra expr = do
        return $ StatNode (pra, StatInvoke expr)


gramExprList :: TokParser s (SourceRange, [ExprNode], Maybe ExprNode)
gramExprList = do
    tinit@(ExprNode(pra, _)) <- gramExpr
    choice [
        do
            _ <- tokSymbol ","
            (prb, rest, mlast) <- gramExprList
            return $ (pra <> prb, tinit:rest, mlast),
        if isMultretExpr tinit
            then return $ (pra, [], Just tinit)
            else return $ (pra, [tinit], Nothing)]


gramExpr :: TokParser s ExprNode
gramExpr = do
    readOr

    where

    readPower :: TokParser s ExprNode
    readPower = do
        base@(ExprNode (pra, _)) <- gramTermExpr
        choice [
            do
                _ <- tokSymbol "^"
                eexp@(ExprNode (prb, _)) <- readPrefix
                return $
                    ExprNode (pra <> prb, ExprBinary BinaryPower base eexp),
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
            choice [
                binaryOp (tokSymbol "<<") (ExprBinary BinaryLShift),
                binaryOp (tokSymbol ">>") (ExprBinary BinaryRShift)]

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
        _ <- parser
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
            _ <- tokKeyword "function"
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
    literal parser construct = do
        (pr, v) <- parser
        return $ ExprNode (pr, construct v)


gramFunctionBody :: TokParser s ExprNode
gramFunctionBody = do
    gramMethodFunctionBody False


gramMethodFunctionBody :: Bool -> TokParser s ExprNode
gramMethodFunctionBody isMethod = do
    pra <- tokSymbol "("
    (params, vararg) <- gramParamList
    _ <- tokSymbol ")"
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
                _ <- tokSymbol ","
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
        _ <- tokSymbol "["
        key <- gramExpr
        _ <- tokSymbol "]"
        _ <- tokSymbol "="
        value <- gramExpr
        (rest, mlast) <- readTableRest
        return $ ((Just key, value):rest, mlast)

    fieldNamed :: TokParser s ([(Maybe ExprNode, ExprNode)], Maybe ExprNode)
    fieldNamed = do
        try (lookAhead (do
            _ <- tokIdent
            _ <- tokSymbol "="
            return ()))
        (pr, nameid) <- tokIdent
        let key = ExprNode (pr, ExprString nameid)
        _ <- tokSymbol "="
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
                _ <- tokSymbol "," <|> tokSymbol ";"
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
        return $ ExprNode (pra <> prb, ExprGroup expr)

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
        _ <- tokSymbol "["
        index <- gramExpr
        prb <- tokSymbol "]"
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixNameIndex :: ExprNode -> TokParser s ExprNode
    suffixNameIndex base@(ExprNode (pra, _)) = do
        _ <- tokSymbol "."
        (prb, name) <- tokIdent
        let index = ExprNode (prb, ExprString name)
        suffixes $ ExprNode (pra <> prb, ExprIndex base index)

    suffixMethod :: ExprNode -> TokParser s ExprNode
    suffixMethod base@(ExprNode (pra, _)) = do
        _ <- tokSymbol ":"
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
        thead <- gramExpr
        choice [
            do
                _ <- tokSymbol ","
                (ttail, mlast) <- argListRest
                return $ ((thead:ttail), mlast),
            if isMultretExpr thead
                then return $ ([], Just thead)
                else return $ ([thead], Nothing)]

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
