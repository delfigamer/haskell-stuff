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
        makef "coroutine." "create" (\args -> do
            let a:_ = luaExtend 1 args
            func <- parseArg 1 "function" luaToFunction a
            thread <- luaCreateThread func
            return $ [thread]),
        makef "coroutine." "resume" (\args -> do
            let co:vals = luaExtend 1 args
            rret <- luaResume co vals
            case rret of
                Left err -> return $ [LBool False, err]
                Right rets -> return $ LBool True:rets),
        makef "coroutine." "status" (\args -> do
            let co:_= luaExtend 1 args
            status <- luaThreadState co
                (return "running")
                (return "suspended")
                (return "normal")
                (return "dead")
            return $ [LString status]),
        makef "coroutine." "wrap" (\args -> do
            let a:_ = luaExtend 1 args
            func <- parseArg 1 "function" luaToFunction a
            co <- luaCreateThread func
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
            return $ [])]
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
            case a of
                LInteger x -> return $ [LInteger $ abs x]
                LRational x -> return $ [LRational $ abs x]
                LDouble x -> return $ [LDouble $ abs x]
                _ -> luaError $ errArgType 1 "number" a),
        makef "math." "exact" (\args -> do
            let a:_ = luaExtend 1 args
            case a of
                LInteger _ -> return $ [a]
                LRational _ -> return $ [a]
                LDouble x -> if isNaN x || isInfinite x
                    then return $ [a]
                    else do
                        let rat = toRational x
                        if denominator rat == 1
                            then return $ [LInteger $ numerator rat]
                            else return $ [LRational $ rat]
                _ -> luaError $ errArgType 1 "number" a),
        return $ (LString "huge", LDouble $ 1/0),
        makef "math." "inexact" (\args -> do
            let a:_ = luaExtend 1 args
            case a of
                LInteger x -> return $ [LDouble $ fromInteger x]
                LRational x -> return $ [LDouble $ fromRational x]
                LDouble _ -> return $ [a]
                _ -> luaError $ errArgType 1 "number" a),
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
                    upper <- maybe
                        (luaError $ errArgType 1 "integer" a)
                        return
                        (luaToInteger a)
                    case upper of
                        0 -> advance nextWord64 (LInteger . toInteger)
                        _ -> advance (nextInteger 1 upper) LInteger
                _ -> do
                    lower <- maybe
                        (luaError $ errArgType 1 "integer" a)
                        return
                        (luaToInteger a)
                    upper <- maybe
                        (luaError $ errArgType 2 "integer" b)
                        return
                        (luaToInteger b)
                    advance (nextInteger lower upper) LInteger),
        makef "math." "randomseed" (\args -> do
            let a:b:_ = luaExtend 2 args
            st <- case (a, b) of
                (LNil, LNil) -> do
                    luaRead randomstate
                _ -> do
                    s1 <- case luaToInteger a of
                        Nothing -> luaError $ errArgType 1 "integer" a
                        Just i1 -> return $ fromInteger i1
                    newst <- case b of
                        LNil -> return $ mkSMGen s1
                        _ -> case luaToInteger b of
                            Nothing -> luaError $ errArgType 2 "integer" b
                            Just i2 -> return $ seedSMGen s1 (fromInteger i2)
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


instance LuaMetatype LuaMetaString where
    lmtAdd (LuaMetaString _ str) other rev def = do
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
                            then luaArithAdd bv av
                            else luaArithAdd av bv
                    else def
            else def
    -- lmtSub :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtMul :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtDiv :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtMod :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtPow :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtUnm :: LuaMetaopUnary (LuaValue q s) q s t
    -- lmtIDiv :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtBAnd :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtBOr :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtBXor :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtBNot :: LuaMetaopUnary (LuaValue q s) q s t
    -- lmtShl :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtShr :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtConcat :: LuaMetaopBinary (LuaValue q s) q s t
    -- lmtLen :: LuaMetaopUnary (LuaValue q s) q s t
    -- lmtEq :: LuaMetaopBinary Bool q s t
    -- lmtLt :: LuaMetaopBinary Bool q s t
    -- lmtLe :: LuaMetaopBinary Bool q s t
    lmtIndex (LuaMetaString slib _) index _ = do
        luaGet slib index
    -- lmtNewIndex
        -- :: t q s
        -- -> LuaValue q s
        -- -> LuaValue q s
        -- -> LuaState q s ()
        -- -> LuaState q s ()
    -- lmtCall
        -- :: t q s
        -- -> [LuaValue q s]
        -- -> LuaState q s [LuaValue q s]
        -- -> LuaState q s [LuaValue q s]
    -- lmtClose :: t q s -> LuaState q s ()
    -- lmtAsString
        -- :: t q s
        -- -> LuaState q s BSt.ByteString
        -- -> LuaState q s BSt.ByteString
    -- lmtShow
        -- :: t q s -> String -> String


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
        _:cs -> func cs
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
                                | maybe True (<count) mlimit -> do
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



enumerate
    :: LuaValue q s
    -> Integer
    -> Maybe Integer
    -> LuaState q s [LuaValue q s]
enumerate list current mfinal = do
    case mfinal of
        Just final -> do
            if current <= final
                then do
                    el <- luaRawGet list (LInteger current)
                    rest <- enumerate list (current+1) mfinal
                    return $ el:rest
                else do
                    return $ []
        Nothing -> do
            el <- luaRawGet list (LInteger current)
            case el of
                LNil -> return $ []
                _ -> do
                    rest <- enumerate list (current+1) mfinal
                    return $ el:rest


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
            elems <- enumerate list first mlast
            parts <- forM (zip [1..] elems) (\(i, el) -> do
                case luaToString el of
                    Just str -> return $ str
                    Nothing -> luaError $ LString $
                        "Expected a string at table index "
                        <> BSt.pack (show (i :: Int))
                        <> ", got a " <> luaTypename el <> " instead")
            return $ [LString $ BSt.intercalate sep parts]),
        makef "table." "pack" (\args -> do
            let keys = map LInteger [1..]
            result <- luaCreateTable $ zip keys args
            luaSet result (LString "n") (LInteger $ toInteger $ length args)
            return $ [result]),
        makef "table." "remove" (\args -> do
            let list:b:_ = luaExtend 2 args
            mpos <- parseArgMaybe 2 "integer" luaToInteger b
            case mpos of
                Just pos -> do
                    let shift n = do
                        el <- luaRawGet list (LInteger (n+1))
                        luaRawSet list (LInteger n) el
                        case el of
                            LNil -> return ()
                            _ -> shift (n+1)
                    el <- luaRawGet list (LInteger pos)
                    shift pos
                    return $ [el]
                Nothing -> do
                    len <- luaRawLen list
                    el <- luaRawGet list (LInteger len)
                    luaRawSet list (LInteger len) LNil
                    return $ [el]),
        makef "table." "sort" (\args -> do
            let list:b:_ = luaExtend 2 args
            let comparer = case b of
                    LNil -> luaCompareLt
                    _ -> (\x y -> do
                        rets <- luaCall b [x, y]
                        case rets of
                            [] -> return $ False
                            r:_ -> return $ luaToBoolean r)
            elems <- enumerate list 1 Nothing
            let qsortm els = do
                case els of
                    [] -> return $ []
                    [x] -> return $ [x]
                    x:ys -> do
                        (leftxs, rightxs) <- partitionEithers <$>
                            forM ys (\y -> do
                                lt <- comparer y x
                                if lt
                                    then return $ Left y
                                    else return $ Right y)
                        lsort <- qsortm leftxs
                        rsort <- qsortm rightxs
                        return $ lsort ++ [x] ++ rsort
            sorted <- qsortm elems
            forM_ (zip [1..] sorted) (\(i, x) ->
                luaRawSet list (LInteger i) x)
            return $ []),
        makef "table." "unpack" (\args -> do
            let list:b:c:_ = luaExtend 3 args
            first <- parseArgDef 2 "integer" luaToInteger 1 b
            mlast <- parseArgMaybe 3 "integer" luaToInteger c
            elems <- enumerate list first mlast
            return $ elems)]
    registerLib table "table" env


lualibBase :: LuaState q s (LuaValue q s)
lualibBase = do
    env <- luaNewTable
    package <- luaCreateTable [
        (LString "config", LString "\\\n;\n?\n\n"),
        (LString "path", LString "?.lua;?\\init.lua"),
        (LString "prefix", LString "")]
    loaded <- luaNewTable
    luaSet package (LString "loaded") loaded
    preload <- luaNewTable
    luaSet package (LString "preload") preload
    searchers <- luaCreateTable =<< sequence [
        makefi 1 "package.loaders[1]" (\args -> do
            let a:_ = luaExtend 1 args
            preloadfunc <- luaRawGet preload a
            case preloadfunc of
                LNil -> return $ [LString $
                    "No field package.preload["
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
            let r:e:_ = luaExtend 2 args
            if luaToBoolean r
                then return $ args
                else luaError e)
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
                _ -> return $ [])
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
            return $ [mt])
    inext <- luaCreateFunction (\args -> do
        let list:b:_ = luaExtend 2 args
        index <- parseArg 2 "integer" luaToInteger b
        value <- luaGet list (LInteger index)
        case value of
            LNil -> return $ []
            _ -> return $ [LInteger (index+1), value])
    uncurry (luaSet env) =<<
        makef "" "ipairs" (\args -> do
            let list:_ = luaExtend 1 args
            return $ [inext, list, LInteger 1])
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
        makef "" "rawget" (\args -> do
            let t:k:_ = luaExtend 2 args
            v <- luaRawGet t k
            return $ [v])
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
                    if idx > 0
                        then return $ drop (fromInteger idx) $ rest
                        else luaError $ errArgRange 1)
    uncurry (luaSet env) =<<
        makef "" "setmetatable" (\args -> do
            let a:b:_ = luaExtend 2 args
            luaSetMetatable a b
            return $ [a])
    uncurry (luaSet env) =<<
        makef "" "tostring" (\args -> do
            let a:_ = luaExtend 1 args
            str <- luaAsString a
            return $ [LString str])
    uncurry (luaSet env) =<<
        makef "" "tonumber" (\args -> do
            let a:b:_ = luaExtend 2 args
            case b of
                LString "exact" -> do
                    case a of
                        LInteger _ -> return $ [a]
                        LRational _ -> return $ [a]
                        LDouble _ -> return $ [a]
                        LString str -> return $ [luaLexNumber str]
                        _ -> return $ [LNil]
                _ -> do
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
