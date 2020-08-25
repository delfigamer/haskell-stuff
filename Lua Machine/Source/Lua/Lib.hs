{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Lua.Lib (
    lualibBase,
    luaopenCoroutine,
    luaopenDebug,
    luaopenIo,
    luaopenMath,
    luaopenOs,
    luaopenString,
    luaopenTable,
    lualibs,
) where


import Control.Monad
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Either
import Data.Function
import Data.List
import Data.Ratio
import Data.Time.Clock
import GHC.Stats
import System.CPUTime
import System.Mem
import System.Random.SplitMix
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Lua.Common
import Lua.Debug
import Lua.Interpret
import Lua.Package
import Lua.SourceRange
import Lua.String


makef
    :: BSt.ByteString
    -> BSt.ByteString
    -> LuaFunction q s
    -> LuaState q s (LuaValue q s, LuaValue q s)
makef prefix name fn = do
    fval <- luaCreateNamedFunction "(intrinsic)" (prefix <> name) fn
    return $ (luaPush name, fval)


makefi
    :: Integer
    -> BSt.ByteString
    -> LuaFunction q s
    -> LuaState q s (LuaValue q s, LuaValue q s)
makefi index name fn = do
    fval <- luaCreateNamedFunction "(intrinsic)" name fn
    return $ (luaPush index, fval)


parseArg
    :: Int
    -> BSt.ByteString
    -> (LuaValue q s -> Maybe t)
    -> LuaValue q s
    -> LuaState q s t
parseArg index expected parser given = do
    case parser given of
        Just r -> return $ r
        Nothing -> luaError $ errArgType index expected given


parseArgDef
    :: Int
    -> BSt.ByteString
    -> (LuaValue q s -> Maybe t)
    -> t
    -> LuaValue q s
    -> LuaState q s t
parseArgDef index expected parser def given =
    case given of
        LNil -> return $ def
        _ -> parseArg index expected parser given


parseArgMaybe
    :: Int
    -> BSt.ByteString
    -> (LuaValue q s -> Maybe t)
    -> LuaValue q s
    -> LuaState q s (Maybe t)
parseArgMaybe index expected parser given =
    case given of
        LNil -> return $ Nothing
        _ -> Just <$> parseArg index expected parser given


pushInt
    :: Int
    -> LuaValue q s
pushInt a = LInteger $ toInteger a


registerLib
    :: LuaValue q s
    -> BSt.ByteString
    -> LuaValue q s
    -> LuaState q s ()
registerLib lib name env = do
    let namev = LString name
    luaSet env namev lib
    package <- luaGet env (LString "package")
    loaded <- luaGet package (LString "loaded")
    luaSet loaded namev lib


luaopenCoroutine :: LuaValue q s -> LuaState q s ()
luaopenCoroutine env = do
    coroutine <- luaCreateTable =<< sequence [
        makef "coroutine." "close" (\args -> do
            let a:_ = luaExtend 1 args
            co <- parseArg 1 "thread" luaToThread a
            merr <- luaThreadClose co
            case merr of
                Nothing -> return $ [LBool True]
                Just err -> return $ [LBool False, err]),
        makef "coroutine." "create" (\args -> do
            let a:_ = luaExtend 1 args
            func <- parseArg 1 "function" luaToFunction a
            thread <- luaCreateThread func
            return $ [thread]),
        makef "coroutine." "isyieldable" (\args -> do
            co <- case args of
                [] -> do
                    ~(LThread _ th, _) <- luaCurrentThread
                    return $ th
                LThread _ th:_ -> return $ th
                a:_ -> luaError $ errArgType 1 "thread" a
            isyieldable <- luaIsYieldable co
            return $ [LBool isyieldable]),
        makef "coroutine." "resume" (\args -> do
            let a:vals = luaExtend 1 args
            co <- parseArg 1 "thread" luaToThread a
            rret <- luaResume co vals
            case rret of
                Left err -> return $ [LBool False, err]
                Right rets -> return $ LBool True:rets),
        makef "coroutine." "running" (\_ -> do
            (th, ismain) <- luaCurrentThread
            return $ [th, LBool ismain]),
        makef "coroutine." "status" (\args -> do
            let a:_= luaExtend 1 args
            co <- parseArg 1 "thread" luaToThread a
            status <- luaThreadState co
                (return "running")
                (return "suspended")
                (return "normal")
                (return "dead")
            return $ [LString status]),
        makef "coroutine." "wrap" (\args -> do
            let a:_ = luaExtend 1 args
            func <- parseArg 1 "function" luaToFunction a
            ~(LThread _ co) <- luaCreateThread func
            let resumer args' = do
                rret <- luaResume co args'
                case rret of
                    Left err -> luaError $ err
                    Right rets -> return $ rets
            resfunc <- luaCreateNamedFunction
                "(intrinsic)" "(coroutine wrapper)" resumer
            return $ [resfunc]),
        makef "coroutine." "yield" (\args -> do
            rets <- luaYield args
            return $ rets)]
    registerLib coroutine "coroutine" env


debugTraceback :: Int -> LuaState q s BSt.ByteString
debugTraceback level
    | level < 0 = return $ "Stack traceback:"
    | otherwise = do
        stack <- luadGetStack
        parts <- mapM infoLine $ stack
        let str = "Stack traceback:" ++ foldr ($) "" (filterrep False id parts)
        return $ BSt.pack str
    where
    infoLine lsf = do
        mcloc <- luaRead $ lsfCurrentLocation lsf
        case (mcloc, lsfDefinition lsf) of
            (Just cloc, Just (_, fname)) -> do
                return $ ("\n\t" ++) . shows cloc . (": " ++) . unpackSt fname
            (Nothing, Just (defloc, fname)) -> do
                return $
                    ("\n\t" ++) . shows (collapseRangeNull defloc)
                        . (": " ++) . unpackSt fname
            (Just cloc, Nothing) -> do
                return $ ("\n\t" ++) . shows cloc
            (Nothing, Nothing) -> do
                return $ id
    unpackSt str rest = BSt.foldr (:) rest str
    filterrep reps _ []
        | reps = [("\n\t\t..." ++)]
        | otherwise = []
    filterrep reps prev (current:rest)
        | prev "" == current "" = filterrep True prev rest
        | reps = ("\t\t..." ++):current:filterrep False current rest
        | otherwise = current:filterrep False current rest


luaopenDebug :: LuaValue q s -> LuaState q s ()
luaopenDebug env = do
    let interactiveMode = do
        luaLiftIO $ putStr $ "luad> "
        input ""
        where
        input buf = do
            rawline <- luaLiftIO BSt.getLine
            let (line, _) = BSt.breakEnd (>'\32') rawline
            case BSt.stripSuffix "\\" line of
                Just line' -> input $ buf <> B.fromStrict line' <> "\n"
                Nothing
                    | line == "cont" -> return ()
                    | otherwise -> do
                        let source = buf <> B.fromStrict line
                        mchunk <- luaLoad "" ("return " <> source) env
                        result <- case mchunk of
                            Right chunk -> luaTry $
                                luaCall chunk []
                            Left _ -> luaTry $ luaDo source env []
                        case result of
                            Left err -> do
                                errstr <- luaAsString err
                                luaLiftIO $ BSt.putStrLn errstr
                            Right [] -> return ()
                            Right rets -> do
                                retstr <- mapM luaAsString rets
                                luaLiftIO $ BSt.putStrLn $
                                    BSt.intercalate "\t" $ retstr
                        interactiveMode
    debug <- luaCreateTable =<< sequence [
        makef "debug." "debug" (\_ -> do
            luaWithErrHandler return $ interactiveMode
            return $ []),
        makef "debug." "gethook" (\_ -> do
            (hookf, callflag, retflag, lineflag) <- luadGetDebugHook
            case hookf of
                LNil -> return $ [LNil]
                _ -> do
                    let !mask =
                               if callflag then "c" else ""
                            <> if retflag then "r" else ""
                            <> if lineflag then "l" else ""
                    return $ [hookf, LString mask]),
        makef "debug." "getinfo" (\args -> do
            let a:_ = luaExtend 1 args
            index <- parseArg 1 "integer" luaToInteger a
            stack <- luadGetStack
            case uncons $ drop (fromInteger index) $ stack of
                Nothing -> return $ [LNil]
                Just (lsf, _) -> do
                    info <- luaCreateTable [
                        (LString "currentline", LInteger $ -1),
                        (LString "name", LString "?"),
                        (LString "namewhat", LString ""),
                        (LString "linedefined", LInteger $ -1),
                        (LString "lastlinedefined", LInteger $ -1),
                        (LString "source", LString ""),
                        (LString "short_src", LString "")]
                    mlocation <- luaRead $ lsfCurrentLocation lsf
                    case mlocation of
                        Just (SourceRange (_, Just (_, _, line, _))) -> do
                            luaRawSet info (LString "currentline")
                                (LInteger $ toInteger line)
                        _ -> return ()
                    case lsfDefinition lsf of
                        Just (SourceRange (source, rowcolrange), name) -> do
                            luaRawSet info (LString "name")
                                (LString name)
                            luaRawSet info (LString "namewhat")
                                (LString "global")
                            luaRawSet info (LString "source")
                                (LString $ BSt.pack source)
                            luaRawSet info (LString "short_src")
                                (LString $ BSt.pack source)
                            case rowcolrange of
                                Just (row1, _, row2, _) -> do
                                    luaRawSet info (LString "linedefined")
                                        (LInteger $ toInteger row1)
                                    luaRawSet info (LString "lastlinedefined")
                                        (LInteger $ toInteger row2)
                                _ -> return ()
                        _ -> return ()
                    return $ [info]),
        makef "debug." "setcstacklimit" (\args -> do
            let a:_ = luaExtend 1 args
            newlimit <- parseArg 1 "integer" luaToInteger a
            moldlimit <- luadSetStackLimit $ fromInteger newlimit
            case moldlimit of
                Nothing -> return $ [LBool False]
                Just oldlimit -> return $ [LInteger $ toInteger oldlimit]),
        makef "debug." "sethook" (\args -> do
            let a:b:_ = luaExtend 2 args
            mhookf <- parseArgMaybe 1 "function" luaToFunction a
            mask <- parseArgDef 2 "string" luaToString "" b
            case mhookf of
                Nothing -> luadSetDebugHook (LNil, False, False, False)
                Just _ -> do
                    let !callflag = BSt.elem 'c' mask
                    let !returnflag = BSt.elem 'r' mask
                    let !lineflag = BSt.elem 'l' mask
                    if callflag || returnflag || lineflag
                        then luadSetDebugHook $
                            (a, callflag, returnflag, lineflag)
                        else luadSetDebugHook (LNil, False, False, False)
            return $ []),
        makef "debug." "setmetatable" (\args -> do
            let a:b:_ = luaExtend 2 args
            luadSetMetatable a b
            return $ [a]),
        makef "debug." "traceback" (\args -> do
            let a:b:_ = luaExtend 2 args
            case a of
                LNil -> do
                    level <- parseArgDef 2 "integer" luaToInteger 1 b
                    tb <- debugTraceback $ fromInteger level
                    return $ [LString $ tb]
                _
                    | Just message <- luaToString a -> do
                        level <- parseArgDef 2 "integer" luaToInteger 1 b
                        tb <- debugTraceback $ fromInteger level
                        return $ [LString $ message <> "\n" <> tb]
                    | otherwise -> return $ [a])]
    registerLib debug "debug" env


luaopenIo :: LuaValue q s -> LuaState q s ()
luaopenIo env = do
    io <- luaCreateTable =<< sequence [
        makef "io." "write" (\args -> do
            luaLiftIO $
                forM_ args (\arg -> do
                    case arg of
                        LString str -> BSt.putStr str
                        _ -> putStr $ show arg)
            return $ [])]
    registerLib io "io" env


mathFMod :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
mathFMod a b = do
    luaToNumber2 a b
        (luaError $ errArgType 1 "number" a)
        (luaError $ errArgType 2 "number" b)
        doInteger
        doRational
        doDouble
    where
    doInteger x y =
        if y == 0
            then luaError $ errDivideZero
            else return $ LInteger $ rem x y
    doRational x y =
        if y == 0
            then doDouble (fromRational x) (fromRational y)
            else return $ LRational $ x - y * fromInteger (truncate (x / y))
    doDouble x y =
        return $ LDouble $
            if isInfinite y
                then x
                else let r = x / y in
                    if isNaN r || isInfinite r
                        then (0/0)
                        else x - y * fromInteger (truncate r)


luaopenMath :: LuaValue q s -> LuaState q s ()
luaopenMath env = do
    mgen <- luaTry $ luaLiftIO $ newSMGen
    randomstate <- case mgen of
        Left _ -> do
            luaAlloc $ mkSMGen 1
        Right g2 -> do
            luaAlloc $ g2
    math <- luaCreateTable =<< sequence [
        makef "math." "abs" (\args -> do
            let a:_ = luaExtend 1 args
            luaToNumber a
                (luaError $ errArgType 1 "number" a)
                (\i -> return $ [LInteger $ abs i])
                (\q -> return $ [LRational $ abs q])
                (\d -> return $ [LDouble $ abs d])),
        makef "math." "ceil" (\args -> do
            let a:_ = luaExtend 1 args
            luaToNumber a
                (luaError $ errArgType 1 "number" a)
                (\i -> return $ [LInteger i])
                (\q -> return $ [LInteger $ ceiling q])
                (\d -> if isNaN d || isInfinite d
                    then return $ [LDouble d]
                    else return $ [LInteger $ ceiling d])),
        makef "math." "exact" (\args -> do
            let a:_ = luaExtend 1 args
            luaToNumber a
                (luaError $ errArgType 1 "number" a)
                (\i -> return $ [LInteger i])
                (\q -> if denominator q == 1
                    then return $ [LInteger $ numerator q]
                    else return $ [LRational q])
                (\d -> if isNaN d || isInfinite d
                    then return $ [LDouble d]
                    else do
                        let q = toRational d
                        if denominator q == 1
                            then return $ [LInteger $ numerator q]
                            else return $ [LRational $ q])),
        makef "math." "floor" (\args -> do
            let a:_ = luaExtend 1 args
            luaToNumber a
                (luaError $ errArgType 1 "number" a)
                (\i -> return $ [LInteger i])
                (\q -> return $ [LInteger $ floor q])
                (\d -> if isNaN d || isInfinite d
                    then return $ [LDouble d]
                    else return $ [LInteger $ floor d])),
        makef "math." "fmod" (\args -> do
            let a:b:_ = luaExtend 2 args
            r <- mathFMod a b
            return $ [r]),
        return $ (LString "huge", LDouble $ 1/0),
        makef "math." "inexact" (\args -> do
            let a:_ = luaExtend 1 args
            luaToNumber a
                (luaError $ errArgType 1 "number" a)
                (\i -> return $ [LDouble $ fromInteger i])
                (\q -> return $ [LDouble $ fromRational q])
                (\d -> return $ [LDouble d])),
        makef "math." "max" (\args -> do
            case args of
                [] -> luaError $ LString "Expected any values, got none"
                first:rest -> do
                    r <- foldM
                        (\current next -> do
                            lt <- luaCompareLt current next
                            if lt
                                then return $ next
                                else return $ current)
                        first
                        rest
                    return $ [r]),
        return $ (LString "maxinteger", LInteger $ 2 ^ (63::Int) - 1),
        return $ (LString "mininteger", LInteger $ - 2 ^ (63::Int)),
        return $ (LString "pi", LDouble $ pi),
        makef "math." "random" (\args -> do
            let a:b:_ = luaExtend 2 args
            let advance genf wrapf = (do
                (r, newst) <- genf <$> luaRead randomstate
                luaWrite randomstate $ newst
                return $ [wrapf r])
            case (a, b) of
                (LNil, LNil) -> do
                    advance nextDouble LDouble
                (_, LNil) -> do
                    upper <- parseArg 1 "integer" luaToInteger a
                    case upper of
                        0 -> advance nextWord64 (LInteger . toInteger)
                        _ -> advance (nextInteger 1 upper) LInteger
                _ -> do
                    lower <- parseArg 1 "integer" luaToInteger a
                    upper <- parseArg 2 "integer" luaToInteger b
                    advance (nextInteger lower upper) LInteger),
        makef "math." "randomseed" (\args -> do
            let a:b:_ = luaExtend 2 args
            st <- case (a, b) of
                (LNil, LNil) -> do
                    luaRead randomstate
                _ -> do
                    s1 <- parseArg 1 "integer" luaToInteger a
                    ms2 <- parseArgMaybe 2 "integer" luaToInteger b
                    let newst = case ms2 of
                            Nothing -> mkSMGen (fromInteger s1)
                            Just s2 -> seedSMGen
                                (fromInteger s1) (fromInteger s2)
                    luaWrite randomstate $ newst
                    return $ newst
            let (s1, s2) = unseedSMGen st
            return $ [
                LInteger $ toInteger s1,
                LInteger $ toInteger s2]),
        makef "math." "sin" (\args -> do
            let a:_ = luaExtend 1 args
            x <- parseArg 1 "number" luaToDouble a
            return $ [LDouble $ sin x]),
        makef "math." "tointeger" (\args -> do
            let a:_ = luaExtend 1 args
            case luaToInteger a of
                Just i -> return $ [LInteger i]
                Nothing -> return $ [LNil]),
        makef "math." "type" (\args -> do
            let a:_ = luaExtend 1 args
            case a of
                LInteger _ -> return $ [LString "integer"]
                LRational _ -> return $ [LString "float", LString "exact"]
                LDouble _ -> return $ [LString "float", LString "inexact"]
                _ -> return $ [LNil])]
    registerLib math "math" env


newtype LuaUTCTime q s = LuaUTCTime UTCTime


instance LuaMetatype LuaUTCTime where
    lmtAsString (LuaUTCTime x) _ = return $ BSt.pack $ show x
    lmtShow (LuaUTCTime x) _ = show x


luaopenOs :: LuaValue q s -> LuaState q s ()
luaopenOs env = do
    let pico = 1000000000000
    os <- luaCreateTable =<< sequence [
        makef "os." "clock" (\_ -> do
            picosec <- luaLiftIO $ getCPUTime
            let sec = picosec % pico
            return $ [LRational sec]),
        makef "os." "setlocale" (\_ -> do
            return $ [LString "hs"]),
        makef "os." "time" (\_ -> do
            ut <- luaLiftIO $ getCurrentTime
            utv <- luaCreateUserdata $ LuaUTCTime ut
            return $ [utv])]
    registerLib os "os" env


data LuaMetaString q s = LuaMetaString
    (LuaValue q s)
    BSt.ByteString


metaStringUnary
    :: (LuaValue q s -> LuaState q s t)
    -> LuaMetaString q s
    -> LuaState q s t
    -> LuaState q s t
metaStringUnary op (LuaMetaString _ str) def = do
    let av = luaLexNumber str
    if luaIsNumber av
        then op av
        else def


metaStringBinary
    :: (LuaValue q s -> LuaValue q s -> LuaState q s t)
    -> LuaMetaString q s
    -> LuaValue q s
    -> Bool
    -> LuaState q s t
    -> LuaState q s t
metaStringBinary op (LuaMetaString _ str) other rev def = do
    let av = luaLexNumber str
    if luaIsNumber av
        then do
            let bv = do
                case other of
                    LString ostr -> luaLexNumber ostr
                    _ -> other
            if luaIsNumber bv
                then do
                    if rev
                        then op bv av
                        else op av bv
                else def
        else def


instance LuaMetatype LuaMetaString where
    lmtAdd = metaStringBinary luaArithAdd
    lmtSub = metaStringBinary luaArithSub
    lmtMul = metaStringBinary luaArithMul
    lmtDiv = metaStringBinary luaArithDiv
    lmtMod = metaStringBinary luaArithMod
    lmtPow = metaStringBinary luaArithPow
    lmtUnm = metaStringUnary luaArithUnm
    lmtIDiv = metaStringBinary luaArithIDiv
    lmtIndex (LuaMetaString slib _) index _ = do
        luaGet slib index


gsubReplString
    :: BSt.ByteString
    -> [LuaValue q s]
    -> LuaState q s (LuaValue q s)
gsubReplString alltemplate allcaps = do
    case allcaps of
        [c] -> process [c, c] id alltemplate
        _ -> process allcaps id alltemplate
    where
    process caps buf template = do
        case BSt.break (=='%') template of
            (left, right)
                | BSt.null right -> do
                    return $ LString $ BSt.concat $ buf [left]
                | BSt.length right < 2 -> do
                    invalidPercent
                | otherwise -> do
                    case BSt.index right 1 of
                        ich
                            | '0' <= ich && ich <= '9' -> do
                                cap <- getCap caps $
                                    fromEnum ich - fromEnum '0'
                                process caps
                                    (buf . (left:) . (cap:))
                                    (BSt.drop 2 right)
                            | ich == '%' -> do
                                let !part = BSt.take (BSt.length left + 1) $
                                        template
                                process caps
                                    (buf . (part:))
                                    (BSt.drop 2 right)
                            | otherwise -> do
                                invalidPercent
    getCap caps i = do
        case uncons $ drop i $ caps of
            Just (cap, _) -> do
                case cap of
                    LNil -> invalidCapture
                    _ -> luaAsString cap
            Nothing -> invalidIndex i
    invalidCapture = luaError $ LString $
        "Unfinished capture"
    invalidIndex i = luaError $ LString $ BSt.pack $
        "Invalid capture index %" ++ show i
    invalidPercent = luaError $ LString $
        "Invalid use of a % in replacement string"


gsubReplTable
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s (LuaValue q s)
gsubReplTable table caps = do
    let key = case caps of
            [c] -> c
            _:c:_ -> c
            _ -> undefined
    luaGet table key


gsubReplFunction
    :: LuaFunction q s
    -> [LuaValue q s]
    -> LuaState q s (LuaValue q s)
gsubReplFunction func caps = do
    rets <- case caps of
        _:c:cs -> func (c:cs)
        _ -> func caps
    case rets of
        ret:_ -> return $ ret
        _ -> return $ LNil


luaopenString :: LuaValue q s -> LuaState q s ()
luaopenString env = do
    string <- luaCreateTable =<< sequence [
        makef "string." "byte" (\args -> do
            let a:b:c:_ = luaExtend 3 args
            source <- parseArg 1 "string" luaToString a
            first <- parseArgDef 2 "integer" luaToInteger 1 b
            final <- parseArgDef 3 "integer" luaToInteger first c
            let spos = stringOffset source first
            let epos = stringOffset source final + 1
            let sub = BSt.drop spos $ BSt.take epos $ source
            return $ map (pushInt . fromEnum) $ BSt.unpack sub),
        makef "string." "char" (\args -> do
            chars <- sequence $ zipWith
                (\a i -> do
                    x <- parseArg i "integer" luaToInteger a
                    if 0 <= x && x <= 255
                        then return $ (toEnum (fromEnum x) :: Char)
                        else luaError $ errArgRange i)
                args
                [1..]
            return $ [LString $ BSt.pack chars]),
        makef "string." "find" (\args -> do
            let a:b:c:d:_ = luaExtend 4 args
            source <- parseArg 1 "string" luaToString a
            pattern <- parseArg 2 "string" luaToString b
            first <- parseArgDef 3 "integer" luaToInteger 1 c
            let plain = luaToBoolean d
            let pos = stringOffset source first
            case () of
                _
                    | BSt.null pattern && pos <= BSt.length source -> do
                        return $ [pushInt (pos+1), pushInt pos]
                    | BSt.null pattern -> do
                        return $ [LNil]
                    | plain -> do
                        let sourcetail = BSt.drop pos $ source
                        let (lsub, rsub) = BSt.breakSubstring pattern sourcetail
                        if BSt.null rsub
                            then return $ [LNil]
                            else do
                                let offset = BSt.length lsub + pos
                                let plen = BSt.length pattern
                                return $ [
                                    pushInt (offset+1),
                                    pushInt (offset+plen)]
                    | otherwise -> do
                        parts <- luaBreakString False source pattern pos
                        case parts of
                            (Left before):(Right (LString sub:caps)):_ -> do
                                let offset = BSt.length before
                                let plen = BSt.length sub
                                return $
                                     pushInt (offset+1)
                                    :pushInt (offset+plen)
                                    :caps
                            (Right (LString sub:caps)):_ -> do
                                let plen = BSt.length sub
                                return $
                                     pushInt 1
                                    :pushInt plen
                                    :caps
                            _ -> return $ [LNil]),
        makef "string." "format" (\args -> do
            let a:rest = luaExtend 1 args
            fmt <- parseArg 1 "string" luaToString a
            result <- luaFormat fmt rest
            return $ [LString result]),
        makef "string." "gmatch" (\args -> do
            let a:b:c:_ = luaExtend 3 args
            source <- parseArg 1 "string" luaToString a
            pattern <- parseArg 2 "string" luaToString b
            first <- parseArgDef 3 "integer" luaToInteger 1 c
            let pos = stringOffset source first
            parts <- luaBreakString True source pattern pos
            matchesRef <- luaAlloc $ rights parts
            iterator <- luaCreateFunction (\_ -> do
                matches <- luaRead matchesRef
                case uncons matches of
                    Nothing -> return $ []
                    Just (match, rest) -> do
                        luaWrite matchesRef $ rest
                        case match of
                            [cap] -> return $ [cap]
                            _:caps -> return $ caps
                            _ -> return $ [])
            return $ [iterator]),
        makef "string." "gsub" (\args -> do
            let a:b:c:d:_ = luaExtend 4 args
            source <- parseArg 1 "string" luaToString a
            pattern <- parseArg 2 "string" luaToString b
            repl <- case c of
                LFunction _ ff -> return $ gsubReplFunction ff
                LTable _ _ -> return $ gsubReplTable c
                _
                    | Just str <- luaToString c -> return $ gsubReplString str
                    | otherwise -> luaError $
                        errArgType 3 "string, function or table" c
            mlimit <- parseArgMaybe 4 "integer" luaToInteger d
            allparts <- luaBreakString False source pattern 0
            let process parts count buf = do
                case parts of
                    part:rest -> do
                        (sub, count') <- case part of
                            Left str -> return $ (str, count)
                            Right caps@(LString orig:_)
                                | maybe True (count<) mlimit -> do
                                    cret <- repl caps
                                    case cret of
                                        LNil -> return $ (orig, count+1)
                                        LBool False -> return $ (orig, count+1)
                                        _ -> case luaToString cret of
                                            Just str -> return $ (str, count+1)
                                            Nothing -> luaError $ LString $
                                                   "Attempt to pass a "
                                                <> luaTypename cret
                                                <> " as a replacement value"
                                | otherwise -> do
                                    return $ (orig, count)
                            _ -> undefined
                        process rest count' (buf . (sub:))
                    [] -> return $ (buf [], count)
            (subs, count) <- process allparts 0 id
            return $ [LString $ BSt.concat $ subs, LInteger count]),
        makef "string." "len" (\args -> do
            let a:_ = luaExtend 1 args
            source <- parseArg 1 "string" luaToString a
            return $ [pushInt $ BSt.length source]),
        makef "string." "lower" (\args -> do
            let a:_ = luaExtend 1 args
            source <- parseArg 1 "string" luaToString a
            let result = BSt.map
                    (\x ->
                        if 'A' <= x && x <= 'Z'
                            then toEnum $
                                fromEnum x + (fromEnum 'a' - fromEnum 'A')
                            else x)
                    source
            return $ [LString $ result]),
        makef "string." "match" (\args -> do
            let a:b:c:_ = luaExtend 3 args
            source <- parseArg 1 "string" luaToString a
            pattern <- parseArg 2 "string" luaToString b
            first <- parseArgDef 3 "integer" luaToInteger 1 c
            let pos = stringOffset source first
            parts <- luaBreakString False source pattern pos
            case uncons $ rights parts of
                Nothing -> return $ [LNil]
                Just (match, _) -> do
                    case match of
                        [cap] -> return $ [cap]
                        _:caps -> return $ caps
                        _ -> return $ [LNil]),
        makef "string." "pack" (\args -> do
            let a:vals = luaExtend 1 args
            pattern <- parseArg 1 "string" luaToString a
            case luaBinpackWrite pattern vals of
                Left err -> luaError $ LString err
                Right output -> return $ [LString output]),
        makef "string." "packsize" (\args -> do
            let a:_ = luaExtend 1 args
            pattern <- parseArg 1 "string" luaToString a
            case luaBinpackSize pattern of
                Left err -> luaError $ LString err
                Right size -> return $ [LInteger $ toInteger size]),
        makef "string." "rep" (\args -> do
            let a:b:c:_ = luaExtend 3 args
            source <- parseArg 1 "string" luaToString a
            count <- parseArg 2 "integer" luaToInteger b
            sep <- parseArgDef 3 "string" luaToString "" c
            when
                ((toInteger (BSt.length source + BSt.length sep) * count)
                > 0x40000000) $
                    luaError $ errArgRange 2
            let icount = fromInteger count
            let result = do
                case () of
                    _
                        | count <= 0 -> ""
                        | BSt.null source && BSt.null sep -> ""
                        | BSt.null source -> do
                            BSt.concat $ replicate (icount-1) $ sep
                        | BSt.null sep -> do
                            BSt.concat $ replicate icount $ source
                        | otherwise -> do
                            BSt.intercalate sep $ replicate icount $ source
            return $ [LString result]),
        makef "string." "reverse" (\args -> do
            let a:_ = luaExtend 3 args
            source <- parseArg 1 "string" luaToString a
            return $ [LString $ BSt.reverse source]),
        makef "string." "sub" (\args -> do
            let a:b:c:_ = luaExtend 3 args
            source <- parseArg 1 "string" luaToString a
            first <- parseArg 2 "integer" luaToInteger b
            mfinal <- parseArgMaybe 3 "integer" luaToInteger c
            let spos = stringOffset source first
            let result = do
                case mfinal of
                    Nothing -> do
                        BSt.drop spos $ source
                    Just final -> do
                        let epos = stringOffset source final + 1
                        BSt.drop spos $ BSt.take epos $ source
            return $ [LString result]),
        makef "string." "unpack" (\args -> do
            let a:b:c:_ = luaExtend 3 args
            pattern <- parseArg 1 "string" luaToString a
            source <- parseArg 2 "string" luaToString b
            first <- parseArgDef 3 "integer" luaToInteger 1 c
            let spos = stringOffset source first
            case luaBinpackRead pattern source spos of
                Left err -> luaError $ LString err
                Right (valbuf, epos) -> return $
                    valbuf [LInteger $ toInteger epos + 1]),
        makef "string." "upper" (\args -> do
            let a:_ = luaExtend 1 args
            source <- parseArg 1 "string" luaToString a
            let result = BSt.map
                    (\x ->
                        if 'a' <= x && x <= 'z'
                            then toEnum $
                                fromEnum x - (fromEnum 'a' - fromEnum 'A')
                            else x)
                    source
            return $ [LString $ result])]
    luaSetStringMetatype $ LuaMetaString string
    registerLib string "string" env
    where
    stringOffset source n
        | i >= 0 = i - 1
        | otherwise = i + BSt.length source
        where
        i = fromInteger n


integerLen :: LuaValue q s -> LuaState q s Integer
integerLen list = do
    lenvalue <- luaLen list
    case luaToInteger lenvalue of
        Just i -> return $ i
        Nothing -> luaError $ errLenType


luaopenTable :: LuaValue q s -> LuaState q s ()
luaopenTable env = do
    table <- luaCreateTable =<< sequence [
        makef "table." "concat" (\args -> do
            let list:b:c:d:_ = luaExtend 4 args
            sep <- case b of
                LNil -> return ""
                _ -> luaAsString b
            first <- parseArgDef 3 "integer" luaToInteger 1 c
            mlast <- parseArgMaybe 4 "integer" luaToInteger d
            lastn <- case mlast of
                Just i -> return $ i
                Nothing -> integerLen list
            parts <- forM [first..lastn] (\i -> do
                el <- luaGet list (LInteger i)
                case luaToString el of
                    Just str -> return $ str
                    Nothing -> luaError $ LString $
                        "Expected a string at table index "
                        <> BSt.pack (show i)
                        <> ", got a " <> luaTypename el <> " instead")
            return $ [LString $ BSt.intercalate sep parts]),
        makef "table." "insert" (\args -> do
            (list, len, pos, value) <- case args of
                [] -> luaError $ errArgType 1 "table" LNil
                [_] -> luaError $ errNoArg 1 "value"
                [list, b] -> do
                    len <- integerLen list
                    return $ (list, len, len+1, b)
                list:a:b:_ -> do
                    len <- integerLen list
                    pos <- parseArg 2 "integer" luaToInteger a
                    unless (1 <= pos && pos <= (len + 1)) $
                        luaError $ errArgRange 2
                    return $ (list, len, pos, b)
            flip fix len $ \loop i -> do
                if i < pos
                    then return ()
                    else do
                        prev <- luaGet list (LInteger $ i)
                        luaSet list (LInteger $ i + 1) prev
                        loop $ i-1
            luaSet list (LInteger pos) value
            return $ []),
        makef "table." "pack" (\args -> do
            let keys = map LInteger [1..]
            result <- luaCreateTable $ zip keys args
            luaSet result (LString "n") (LInteger $ toInteger $ length args)
            return $ [result]),
        makef "table." "remove" (\args -> do
            let list:b:_ = luaExtend 2 args
            len <- integerLen list
            pos <- parseArgDef 2 "integer" luaToInteger len b
            unless (False
                    || 1 <= pos && pos <= (len + 1)
                    || len == 0 && pos == 0) $
                luaError $ errArgRange 2
            value <- luaGet list (LInteger pos)
            flip fix pos $ \loop i -> do
                if i >= len
                    then luaSet list (LInteger i) LNil
                    else do
                        next <- luaGet list (LInteger $ i+1)
                        luaSet list (LInteger i) next
                        loop $ i+1
            return $ [value]),
        makef "table." "sort" (\args -> do
            let list:b:_ = luaExtend 2 args
            let comparer = case b of
                    LNil -> luaCompareLt
                    _ -> \x y -> do
                        rets <- luaCall b [x, y]
                        case rets of
                            [] -> return $ False
                            r:_ -> return $ luaToBoolean r
            len <- integerLen list
            listelems <- forM [1..len] $ \i -> luaGet list (LInteger i)
            sorted <- flip fix listelems $ \rec elems -> do
                case elems of
                    [] -> return $ []
                    pivot:rest -> do
                        prest <- forM rest $ \el -> do
                            cr <- comparer el pivot
                            if cr
                                then return $ Left el
                                else return $ Right el
                        let (less, greater) = partitionEithers prest
                        left <- rec less
                        right <- rec greater
                        return $ left ++ [pivot] ++ right
            zipWithM_ (\i el -> luaSet list (LInteger i) el) [1..] sorted
            return $ []),
        makef "table." "unpack" (\args -> do
            let list:b:c:_ = luaExtend 3 args
            first <- parseArgDef 3 "integer" luaToInteger 1 b
            mlast <- parseArgMaybe 4 "integer" luaToInteger c
            lastn <- case mlast of
                Just i -> return $ i
                Nothing -> integerLen list
            elems <- forM [first..lastn] (\i -> luaGet list (LInteger i))
            return $ elems)]
    registerLib table "table" env


lualibBase :: LuaState q s (LuaValue q s)
lualibBase = do
    env <- luaNewTable
    package <- luaCreateTable [
        (LString "config", LString "\\\n;\n?\n\n"),
        (LString "path", LString "?.lua;?\\init.lua"),
        (LString "prefix", LString "")]
    loaded <- luaCreateTable [
        (LString "package", package)]
    luaSet package (LString "loaded") loaded
    preload <- luaNewTable
    luaSet package (LString "preload") preload
    searchers <- luaCreateTable =<< sequence [
        makefi 1 "package.loaders[1]" (\args -> do
            let a:_ = luaExtend 1 args
            preloadfunc <- luaRawGet preload a
            case preloadfunc of
                LNil -> return $ [LString $
                    "\nNo field package.preload["
                        <> BSt.pack (show a) <> "]"]
                _ -> return $ [preloadfunc, a]),
        makefi 2 "package.loaders[2]" (\args -> do
            let a:_ = luaExtend 1 args
            case luaToString a of
                Nothing -> return $ [LString "Module name is not a string"]
                Just name -> do
                    prefix <- luaAsString =<<
                        luaRawGet package (LString "prefix")
                    path <- luaAsString =<< luaRawGet package (LString "path")
                    sr <- lpkSearchPath name prefix path "." "\\"
                    case sr of
                        Left err -> return $ [LString err]
                        Right found -> do
                            let fname = BSt.unpack found
                            lret <- luaLoadFile fname env
                            case lret of
                                Left err -> luaError err
                                Right chunk -> return $ [chunk, a])]
    luaSet package (LString "searchers") searchers
    uncurry (luaSet package) =<<
        makef "package." "searchpath" (\args -> do
            let a:b:c:d:e:_ = luaExtend 5 args
            name <- parseArg 1 "string" luaToString a
            path <- parseArg 2 "string" luaToString b
            sep <- parseArgDef 3 "string" luaToString "." c
            rep <- parseArgDef 4 "string" luaToString "\\" d
            prefix <- case e of
                LNil -> luaAsString =<< luaRawGet package (LString "prefix")
                _ -> parseArg 5 "string" luaToString e
            result <- lpkSearchPath name prefix path sep rep
            case result of
                Left err -> return $ [LNil, LString err]
                Right found -> return $ [LString found])
    uncurry (luaSet env) =<<
        makef "" "assert" (\args -> do
            case args of
                [] -> luaError $ errNoArg 1 "value"
                [r]
                    | luaToBoolean r -> return $ args
                    | otherwise -> luaError $ LString "Assertion failed"
                r:e:_
                    | luaToBoolean r -> return $ args
                    | otherwise -> luaError e)
    uncurry (luaSet env) =<<
        makef "" "collectgarbage" (\args -> do
            let a:_ = luaExtend 1 args
            case a of
                LString "collect" -> do
                    luaLiftST $ unsafeIOToST $ performMajorGC
                    return $ []
                LNil -> do
                    luaLiftST $ unsafeIOToST $ performMajorGC
                    return $ []
                LString "count" -> do
                    cnt <- luaLiftST $ unsafeIOToST (do
                        enabled <- getRTSStatsEnabled
                        if enabled
                            then do
                                stats <- getRTSStats
                                return $ max_live_bytes stats
                            else return $ 1000)
                    return $ [LRational $ toInteger cnt % 1024]
                LString "step" -> do
                    luaLiftST $ unsafeIOToST $ performMinorGC
                    return $ []
                LString "isrunning" -> do
                    return $ [LBool True]
                _ -> luaError $ LString "Invalid option")
    uncurry (luaSet env) =<<
        makef "" "dofile" (\args -> do
            let a:as = luaExtend 1 args
            sourcename <- parseArg 1 "string" luaToString a
            prefix <- luaAsString =<< luaRawGet package (LString "prefix")
            let fname = BSt.unpack $ prefix <> sourcename
            lret <- luaLoadFile fname env
            case lret of
                Left err -> luaError $ err
                Right chunk -> luaCall chunk as)
    uncurry (luaSet env) =<<
        makef "" "error" (\args -> do
            let e:_ = luaExtend 1 args
            luaError e)
    uncurry (luaSet env) =<<
        makef "" "getmetatable" (\args -> do
            let a:_ = luaExtend 1 args
            mt <- luaGetMetatable a
            case mt of
                LTable _ _ -> do
                    mt2 <- luaRawGet mt (LString "__metatable")
                    case mt2 of
                        LNil -> return $ [mt]
                        _ -> return $ [mt2]
                _ -> return $ [mt])
    inext <- luaCreateFunction (\args -> do
        let list:b:_ = luaExtend 2 args
        index <- parseArg 2 "integer" luaToInteger b
        let nexti = LInteger $ index + 1
        value <- luaGet list nexti
        case value of
            LNil -> return $ []
            _ -> return $ [nexti, value])
    uncurry (luaSet env) =<<
        makef "" "ipairs" (\args -> do
            let list:_ = luaExtend 1 args
            case list of
                LNil -> luaError $ errArgType 1 "table" list
                _ -> return ()
            return $ [inext, list, LInteger 0])
    uncurry (luaSet env) =<<
        makef "" "load" (\args -> do
            let a:b:c:cs = luaExtend 3 args
            esource <- case a of
                LString bstr -> return $ Right $ B.fromStrict bstr
                LFunction _ ff -> luaTry $ lpkLoadSource ff
                _ -> luaError $ errArgType 1 "function or string" a
            chunkname <- parseArgDef 2 "string" luaToString "=(load)" b
            _mode <- parseArgDef 3 "string" luaToString "" c
            let funcenv = case cs of
                    [] -> env
                    d:_ -> d
            case esource of
                Left err -> return $ [LNil, err]
                Right source -> do
                    lret <- luaLoad (BSt.unpack chunkname) source funcenv
                    case lret of
                        Left err -> return $ [LNil, err]
                        Right chunk -> return $ [chunk])
    uncurry (luaSet env) =<<
        makef "" "loadfile" (\args -> do
            let a:b:bs = luaExtend 2 args
            sourcename <- parseArg 1 "string" luaToString a
            prefix <- luaAsString =<< luaRawGet package (LString "prefix")
            _mode <- parseArgDef 2 "string" luaToString "" b
            let funcenv = case bs of
                    [] -> env
                    c:_ -> c
            let fname = BSt.unpack $ prefix <> sourcename
            lret <- luaLoadFile fname funcenv
            case lret of
                Left err -> return $ [LNil, err]
                Right chunk -> return $ [chunk])
    uncurry (luaSet env) =<<
        makef "" "pairs" (\args -> do
            let list:_ = luaExtend 1 args
            case list of
                LTable _ _ -> return ()
                _ -> luaError $ errArgType 1 "table" list
            listmeta <- luaGetMetatable list
            metapairs <- luaXTry return $ luaGet listmeta (LString "__pairs")
            case metapairs of
                Right (LFunction _ ff) -> do
                    take 3 <$> ff [list]
                _ -> do
                    piter <- luaAlloc =<< luaPairs list
                    stepfunc <- luaCreateFunction (\_ -> do
                        iter <- luaRead piter
                        luaNext iter
                            (return $ [])
                            (\iter2 key value -> do
                                luaWrite piter iter2
                                return $ [key, value]))
                    return $ [stepfunc])
    uncurry (luaSet env) =<<
        makef "" "pcall" (\args -> do
            let a:as = luaExtend 1 args
            res <- luaTry $ luaCall a as
            case res of
                Left err -> return $ [LBool False, err]
                Right rets -> return $ LBool True:rets)
    uncurry (luaSet env) =<<
        makef "" "print" (\args -> do
            parts <- mapM luaAsString args
            let line = intercalate "\t" $ map BSt.unpack parts
            luaLiftIO $ putStrLn $ line
            return $ [])
    uncurry (luaSet env) =<<
        makef "" "rawequal" (\args -> do
            let a:b:_ = luaExtend 2 args
            eq <- luaRawEqual a b
            return $ [LBool $ eq])
    uncurry (luaSet env) =<<
        makef "" "rawget" (\args -> do
            let t:k:_ = luaExtend 2 args
            v <- luaRawGet t k
            return $ [v])
    uncurry (luaSet env) =<<
        makef "" "rawlen" (\args -> do
            let t:_ = luaExtend 1 args
            len <- luaRawLen t
            return $ [LInteger $ len])
    uncurry (luaSet env) =<<
        makef "" "rawset" (\args -> do
            let t:k:v:_ = luaExtend 3 args
            luaRawSet t k v
            return $ [])
    uncurry (luaSet env) =<<
        makef "" "require" (\args -> do
            let a:_ = luaExtend 1 args
            _ <- parseArg 1 "string" luaToString a
            modvalue <- lpkRequire loaded searchers a
            return $ [modvalue])
    uncurry (luaSet env) =<<
        makef "" "select" (\args -> do
            let a:rest = luaExtend 1 args
            case a of
                LString "#" -> do
                    return $ [LInteger $ toInteger $ length $ rest]
                _ -> do
                    idx <- parseArg 1 "integer" luaToInteger a
                    case idx of
                        _
                            | idx > 0 -> do
                                return $ drop (fromInteger idx - 1) $ rest
                            | idx < 0
                            , let revl = length rest + fromInteger idx
                            , revl >= 0 -> do
                                return $ drop revl $ rest
                            | otherwise -> do
                                luaError $ errArgRange 1)
    uncurry (luaSet env) =<<
        makef "" "setmetatable" (\args -> do
            let a:b:_ = luaExtend 2 args
            mt <- luaGetMetatable a
            case mt of
                LTable _ _ -> do
                    mt2 <- luaRawGet mt (LString "__metatable")
                    case mt2 of
                        LNil -> luaSetMetatable a b
                        _ -> luaError $ LString $
                            "Cannot change a protected metatable"
                _ -> luaSetMetatable a b
            return $ [a])
    uncurry (luaSet env) =<<
        makef "" "tostring" (\args -> do
            case args of
                [] -> luaError $ errNoArg 1 "value"
                a:_ -> do
                    str <- luaAsString a
                    return $ [LString str])
    uncurry (luaSet env) =<<
        makef "" "tonumber" (\args -> do
            case args of
                [] -> luaError $ errNoArg 1 "value"
                a:LString "exact":_ -> do
                    case a of
                        LInteger _ -> return $ [a]
                        LRational _ -> return $ [a]
                        LDouble _ -> return $ [a]
                        LString str -> return $ [luaLexNumber str]
                        _ -> return $ [LNil]
                a:as -> do
                    let b:_ = luaExtend 1 as
                    mbase <- parseArgMaybe 2 "integer" luaToInteger b
                    case mbase of
                        Just base -> do
                            unless (2 <= base && base <= 36) $
                                luaError $ errArgRange 2
                            case a of
                                LString source -> do
                                    return $ [luaLexInteger source base]
                                _ -> luaError $ errArgType 1 "string" a
                        Nothing -> do
                            case a of
                                LInteger _ -> return $ [a]
                                LRational _ -> return $ [a]
                                LDouble _ -> return $ [a]
                                LString str -> do
                                    case luaLexNumber str of
                                        LRational q -> return $
                                            [LDouble $ fromRational q]
                                        r -> return $ [r]
                                _ -> return $ [LNil])
    uncurry (luaSet env) =<<
        makef "" "type" (\args -> do
            case args of
                a:_ -> return $ [LString $ luaTypename a]
                _ -> luaError $ LString "Expected a value, none was given")
    uncurry (luaSet env) =<<
        makef "" "warn" (\args -> do
            let a:_ = luaExtend 1 args
            luaWarn a
            return $ [])
    uncurry (luaSet env) =<<
        makef "" "xpcall" (\args -> do
            let a:b:as = luaExtend 2 args
            mhandler <- parseArgMaybe 2 "function" luaToFunction b
            let errh = case mhandler of
                    Just ff -> \e -> do
                        rets <- ff [e]
                        case rets of
                            LNil:_ -> return $ e
                            r:_ -> return $ r
                            _ -> return $ e
                    Nothing -> return
            res <- luaWithErrHandler errh $ luaTry $ luaCall a as
            case res of
                Left err -> return $ [LBool False, err]
                Right rets -> return $ LBool True:rets)
    luaSet env (LString "package") package
    luaSet env (LString "_G") env
    return $ env


lualibs :: LuaState q s (LuaValue q s)
lualibs = do
    env <- lualibBase
    luaopenCoroutine env
    luaopenDebug env
    luaopenIo env
    luaopenMath env
    luaopenOs env
    luaopenString env
    luaopenTable env
    return $ env
