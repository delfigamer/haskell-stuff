{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}


module Lua.Common (
    LuaErrorHandler,
    LuaFunction,
    LuaIterator,
    LuaMetatype(..),
    LuaMetatypeWrapper(..),
    LuaPush(..),
    LuaRef,
    LuaState,
    LuaThread,
    LuaValue(..),
    errArgRange,
    errArgType,
    errDivideZero,
    errLenType,
    errNanIndex,
    errNilIndex,
    errNoArg,
    errNonSuspended,
    errWrongArith1,
    errWrongArith2,
    errWrongBit,
    errWrongCall,
    errWrongCompare,
    errWrongConcat,
    errWrongIO,
    errWrongLen,
    errWrongResume,
    errWrongMetatableOwner,
    errWrongMetatableValue,
    errWrongRangeIter,
    errWrongTable,
    errWrongThreadFunc,
    errWrongThreadStatus,
    errWrongYield,
    errZeroStep,
    luaAlloc,
    luaArithAdd,
    luaArithBAnd,
    luaArithBNot,
    luaArithBOr,
    luaArithBXor,
    luaArithDiv,
    luaArithIDiv,
    luaArithMul,
    luaArithMod,
    luaArithPow,
    luaArithShl,
    luaArithShr,
    luaArithSub,
    luaArithUnm,
    luaAsString,
    luaCall,
    luaCall_,
    luaCloseAfter,
    luaCompareLt,
    luaCompareLe,
    luaCompareEq,
    luaConcat,
    luaCreateFunction,
    luaCreateNamedFunction,
    luaCreateTable,
    luaCreateThread,
    luaCreateUserdata,
    luaCurrentThread,
    luaError,
    luaFromUserdata,
    luaGet,
    luaGetMetatable,
    luaIsNumber,
    luaIsYieldable,
    luaLen,
    luaLexNumber,
    luaLiftIO,
    luaLiftST,
    luaNewTable,
    luaNewThread,
    luaNext,
    luaPCall,
    luaPairs,
    luaRangeIter,
    luaRawEqual,
    luaRawGet,
    luaRawLen,
    luaRawSet,
    luaRead,
    luaResume,
    luaRunIO,
    luaRunST,
    luaRunT,
    luaSet,
    luaSetBoolMetatype,
    luaSetFunctionMetatype,
    luaSetMetatable,
    luaSetNilMetatype,
    luaSetNumberMetatype,
    luaSetStringMetatype,
    luaSetTableMetatype,
    luaSetThreadMetatype,
    luaSetWarnHandler,
    luaThreadClose,
    luaThreadState,
    luaToBoolean,
    luaToDouble,
    luaToInteger,
    luaToFunction,
    luaToNil,
    luaToNumber,
    luaToNumber2,
    luaToRational,
    luaToString,
    luaToThread,
    luaToUserdata,
    luaTry,
    luaTypename,
    luaUncons,
    luaWarn,
    luaWithErrHandler,
    luaWrite,
    luaXPCall,
    luaXTry,
    luaYield,
    pattern (:|),
) where


import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Ratio
import System.IO.Error
import Type.Reflection
import qualified Data.ByteString.Char8 as BSt
import Lua.Core
import Lua.SourceRange


luaAlloc :: a -> LuaState q s (LuaRef q s a)
luaAlloc = lxAlloc


luaArithUnm :: LuaValue q s -> LuaState q s (LuaValue q s)
luaArithUnm a = lxPerformUnary
    lxRawArithUnm lmtUnm a $
        lxError $ errWrongArith1 a

luaArithBNot :: LuaValue q s -> LuaState q s (LuaValue q s)
luaArithBNot a = lxPerformUnary
    lxRawArithBNot lmtBNot a $
        lxError $ errWrongArith1 a

luaArithAdd :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithAdd a b = lxPerformBinary
    (lxRawArithNum (+)) lmtAdd a b $
        lxError $ errWrongArith2 a b

luaArithSub :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithSub a b = lxPerformBinary
    (lxRawArithNum (-)) lmtSub a b $
        lxError $ errWrongArith2 a b

luaArithMul :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithMul a b = lxPerformBinary
    (lxRawArithNum (*)) lmtMul a b $
        lxError $ errWrongArith2 a b

luaArithDiv :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithDiv a b = lxPerformBinary
    lxRawArithDiv lmtDiv a b $
        lxError $ errWrongArith2 a b

luaArithIDiv :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithIDiv a b = lxPerformBinary
    lxRawArithIDiv lmtIDiv a b $
        lxError $ errWrongArith2 a b

luaArithMod :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithMod a b = lxPerformBinary
    lxRawArithMod lmtMod a b $
        lxError $ errWrongArith2 a b

luaArithPow :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithPow a b = lxPerformBinary
    lxRawArithPow lmtPow a b $
        lxError $ errWrongArith2 a b

luaArithBAnd :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithBAnd a b = lxPerformBinary
    (lxRawArithBitwise (.&.)) lmtBAnd a b $
        lxError $ errWrongArith2 a b

luaArithBOr :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithBOr a b = lxPerformBinary
    (lxRawArithBitwise (.|.)) lmtBOr a b $
        lxError $ errWrongArith2 a b

luaArithBXor :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithBXor a b = lxPerformBinary
    (lxRawArithBitwise xor) lmtBXor a b $
        lxError $ errWrongArith2 a b

luaArithShl :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithShl a b = lxPerformBinary
    (lxRawArithBitwise (\x y -> shiftL x (fromInteger y))) lmtShl a b $
        lxError $ errWrongArith2 a b

luaArithShr :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaArithShr a b = lxPerformBinary
    (lxRawArithBitwise (\x y -> shiftR x (fromInteger y))) lmtShr a b $
        lxError $ errWrongArith2 a b


luaAsString
    :: LuaValue q s
    -> LuaState q s BSt.ByteString
luaAsString a = lxAsString a


luaCall
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s [LuaValue q s]
luaCall a args = lxCall a args


luaCall_
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s ()
luaCall_ a args = () <$ lxCall a args


luaCloseAfter
    :: LuaValue q s
    -> LuaState q s t
    -> LuaState q s t
luaCloseAfter a act = do
    lxMetatable a (\x -> do
        lxFinally act $ lmtClose x)


luaCompareLt :: LuaValue q s -> LuaValue q s -> LuaState q s Bool
luaCompareLt a b = lxPerformBinary
    (lxRawCompare (<)) lmtLt a b $
        lxError $ errWrongCompare a b


luaCompareLe :: LuaValue q s -> LuaValue q s -> LuaState q s Bool
luaCompareLe a b = lxPerformBinary
    (lxRawCompare (<=)) lmtLe a b $
        lxError $ errWrongCompare a b


luaCompareEq :: LuaValue q s -> LuaValue q s -> LuaState q s Bool
luaCompareEq a b = lxPerformBinary
    lxRawCompareEq lmtEq a b $
        return $ False


luaConcat :: LuaValue q s -> LuaValue q s -> LuaState q s (LuaValue q s)
luaConcat a b = do
    case (,) <$> luaToString a <*> luaToString b of
        Just (as, bs) -> return $ LString $ as <> bs
        Nothing -> lxPerformMetaopBinary lmtConcat a b $
            lxError $ errWrongConcat a b


luaCreateFunction
    :: LuaFunction q s
    -> LuaState q s (LuaValue q s)
luaCreateFunction fn = do
    cnt <- lxNewId
    return $ LFunction cnt fn


luaCreateNamedFunction
    :: FilePath
    -> BSt.ByteString
    -> LuaFunction q s
    -> LuaState q s (LuaValue q s)
luaCreateNamedFunction modname name fn = do
    let pr = nullRange modname
    locationRef <- lxAlloc (Just pr)
    let lstackFrame = LuaStackFrame {
        lsfDefinition = Just (pr, name),
        lsfCurrentLocation = locationRef,
        lsfUpvalues = [],
        lsfLocals = []}
    let fn' args = lxLocalStack (lstackFrame:) $ fn args
    luaCreateFunction fn'


luaCreateTable
    :: [(LuaValue q s, LuaValue q s)]
    -> LuaState q s (LuaValue q s)
luaCreateTable kv = do
    i <- lxNewId
    table <- lxAllocTable kv
    return $ LTable i table


luaCreateThread
    :: LuaFunction q s
    -> LuaState q s (LuaValue q s)
luaCreateThread f = do
    lxNewThread f


luaCreateUserdata
    :: LuaMetatype t
    => t q s
    -> LuaState q s (LuaValue q s)
luaCreateUserdata x = do
    i <- lxNewId
    return $ LUserdata i x


luaCurrentThread
    :: LuaState q s (LuaValue q s, Bool)
luaCurrentThread = lxCurrentThread


luaError :: LuaValue q s -> LuaState q s a
luaError e = lxError e


luaFromUserdata
    :: LuaValue q s
    -> (forall t . (LuaMetatype t) => t q s -> LuaState q s u)
    -> LuaState q s u
luaFromUserdata a f = do
    lxMetatable a (\x -> f x)


luaGet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
luaGet a b = lxGet a b


luaGetMetatable
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
luaGetMetatable a = do
    lxMetatable a (\x -> return $ lmtMetatable x)


luaIsNumber
    :: LuaValue q s
    -> Bool
luaIsNumber a = do
    case a of
        LInteger _ -> True
        LRational _ -> True
        LDouble _ -> True
        _ -> False


luaIsYieldable
    :: LuaThread q s
    -> LuaState q s Bool
luaIsYieldable = lxIsYieldable


luaLen
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
luaLen a = do
    case a of
        LString x -> return $ LInteger $ toInteger (BSt.length x)
        _ -> lxMetatable a (\x -> lmtLen x $
            case a of
                LTable _ table -> LInteger <$> lxTableLength table
                _ -> lxError $ errWrongLen a)


luaLexNumber :: BSt.ByteString -> LuaValue q s
luaLexNumber buf = do
    readStart $ BSt.unpack buf
    where
    readStart str = do
        case str of
            c:rest | '\9' <= c && c <= '\13' -> readStart rest
            ' ':rest-> readStart rest
            '-':rest -> readPrefix (-1) rest
            '+':rest -> readPrefix 1 rest
            rest -> readPrefix 1 rest
    readPrefix sign str = do
        case str of
            '0':'x':rest -> readAbs sign 16 rest
            '0':'X':rest -> readAbs sign 16 rest
            rest -> readAbs sign 10 rest
    readAbs sign base str = readInteger base str $ readFraction sign base
    readFraction sign base ipart str = do
        case str of
            '.':rest -> readInteger base rest $
                \i -> readExponent sign base ipart (Just i)
            rest -> readExponent sign base ipart Nothing rest
    readExponent sign base ipart fpart str = do
        case (base, str) of
            (10, 'e':rest) -> readExponentValue sign ipart fpart 10 rest
            (10, 'E':rest) -> readExponentValue sign ipart fpart 10 rest
            (16, 'p':rest) -> readExponentValue sign ipart fpart 2 rest
            (16, 'P':rest) -> readExponentValue sign ipart fpart 2 rest
            (_, rest) -> readEnd sign ipart fpart Nothing rest
    readExponentValue sign ipart fpart ebase str = do
        case str of
            '+':rest -> readInteger 10 rest $
                readExponentThen sign ipart fpart ebase True
            '-':rest -> readInteger 10 rest $
                readExponentThen sign ipart fpart ebase False
            rest -> readInteger 10 rest $
                readExponentThen sign ipart fpart ebase True
    readExponentThen sign ipart fpart ebase esign (enum, eden) str = do
        if eden == 1
            then LNil
            else if esign
                then readEnd sign ipart fpart (Just (ebase^enum, 1)) str
                else readEnd sign ipart fpart (Just (1, ebase^enum)) str
    readEnd sign ipart@(inum, iden) fpart epart str = do
        case str of
            "" -> case (fpart, epart) of
                (Just (_, 1), _) | iden == 1 -> LNil
                (Nothing, _) | iden == 1 -> LNil
                (Nothing, Nothing) ->
                    LInteger $ sign * inum
                (Just (fnum, fden), Nothing) ->
                    LRational $ sign * (inum * fden + fnum) % fden
                (Nothing, Just (enum, eden)) ->
                    LRational $ sign * (inum * enum) % eden
                (Just (fnum, fden), Just (enum, eden)) ->
                    LRational $ sign *
                        ((inum * fden + fnum) * enum) % (fden * eden)
            c:rest | '\9' <= c && c <= '\13' ->
                readEnd sign ipart fpart epart rest
            ' ':rest -> readEnd sign ipart fpart epart rest
            _ -> LNil
    readInteger base str cont = do
        readIntegerNext base (0, 1) str cont
    readIntegerNext base (num, den) str cont = do
        case str of
            c:rest
                | '0' <= c && c <= '9' -> do
                    let d = toInteger $ fromEnum c - fromEnum '0'
                    readIntegerNext base (num*base + d, den*base) rest cont
                | base == 16 && 'a' <= c && c <= 'f' -> do
                    let d = toInteger $ fromEnum c - (fromEnum 'a' - 10)
                    readIntegerNext base (num*base + d, den*base) rest cont
                | base == 16 && 'A' <= c && c <= 'F' -> do
                    let d = toInteger $ fromEnum c - (fromEnum 'A' - 10)
                    readIntegerNext base (num*base + d, den*base) rest cont
            rest -> cont (num, den) rest


luaLiftIO
    :: IO t
    -> LuaState q s t
luaLiftIO act = do
    mr <- lxTryLiftIO $ tryIOError act
    case mr of
        Nothing -> lxError $ errWrongIO
        Just (Left ioerr) -> lxError $ LString $ BSt.pack $ show ioerr
        Just (Right r) -> return $ r


luaLiftST
    :: ST s t
    -> LuaState q s t
luaLiftST act = do
    lxLiftST act


luaNewTable
    :: LuaState q s (LuaValue q s)
luaNewTable = luaCreateTable []


luaNewThread
    :: LuaState q s (LuaValue q s)
luaNewThread = luaCreateThread (\start -> do
    case start of
        (LFunction _ f):args -> f args
        a:_ -> lxError $ errWrongThreadFunc a
        _ -> lxError $ errWrongThreadFunc LNil)


luaNext
    :: LuaIterator q s
    -> LuaState q s a
    -> (LuaIterator q s -> LuaValue q s -> LuaValue q s -> LuaState q s a)
    -> LuaState q s a
luaNext = lxTableNext


luaPCall
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
luaPCall a args = lxTry $ lxCall a args


luaPairs
    :: LuaValue q s
    -> LuaState q s (LuaIterator q s)
luaPairs a = do
    case a of
        LTable _ table -> lxTableIter table
        _ -> lxError $ errWrongTable a


class LuaPush a where
    luaPush :: a -> LuaValue q s

instance LuaPush () where
    luaPush () = LNil

instance LuaPush Bool where
    luaPush = LBool

instance LuaPush Integer where
    luaPush = LInteger

instance LuaPush Rational where
    luaPush = LRational

instance LuaPush Double where
    luaPush = LDouble

instance LuaPush BSt.ByteString where
    luaPush = LString

instance LuaPush [Char] where
    luaPush x = LString $ BSt.pack x


luaRangeIter
    :: LuaValue q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
luaRangeIter a b c = do
    luaToNumber2 a c
        (lxError $ errWrongRangeIter)
        (lxError $ errWrongRangeIter)
        onInteger
        onRational
        onDouble
    where
    onInteger start step = do
        when (step == 0) $ lxError $ errZeroStep
        cond <- luaToNumber b
            (lxError $ errWrongRangeIter)
            (\bi -> do
                return $ \i -> comparer step i bi)
            (\bq -> do
                return $ \i -> comparer step (toRational i) bq)
            (\bd -> do
                if isNaN bd || isInfinite bd
                    then return $ \_ -> comparer step 0 bd
                    else do
                        let bq = toRational bd
                        return $ \i -> comparer step (toRational i) bq)
        iterFunc start step cond
    onRational start step = do
        when (step == 0) $ lxError $ errZeroStep
        cond <- luaToNumber b
            (lxError $ errWrongRangeIter)
            (\bi -> do
                let bq = fromInteger bi
                return $ \q -> comparer step q bq)
            (\bq -> return $ \q -> comparer step q bq)
            (\bd -> do
                if isNaN bd || isInfinite bd
                    then return $ \_ -> comparer step 0 bd
                    else do
                        let bq = toRational bd
                        return $ \q -> comparer step q bq)
        iterFunc start step cond
    onDouble start step = do
        when (step == 0) $ lxError $ errZeroStep
        cond <- luaToNumber b
            (lxError $ errWrongRangeIter)
            (\bi -> do
                let bd = fromInteger bi
                return $ \d -> comparer step d bd)
            (\bq -> do
                let bd = fromRational bq
                return $ \d -> comparer step d bd)
            (\bd -> return $ \d -> comparer step d bd)
        iterFunc start step cond
    comparer step
        | step > 0 = (<=)
        | otherwise = (>=)
    iterFunc start step cond = do
        pvalue <- lxAlloc start
        luaCreateFunction $ \_ -> do
            param <- lxRead pvalue
            if cond param
                then do
                    lxWrite pvalue $ param + step
                    return $ [luaPush param]
                else return $ [LNil]


luaRawEqual
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s Bool
luaRawEqual a b = do
    lxRawCompareEq a b $
        return $ False


luaRawGet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
luaRawGet a b = do
    case a of
        LTable _ table -> do
            case b of
                LNil -> return $ LNil
                LDouble d | isNaN d -> return $ LNil
                _ -> lxTableGet table b
        _ -> lxError $ errWrongTable a


luaRawLen
    :: LuaValue q s
    -> LuaState q s Integer
luaRawLen a = do
    lxRawLen a $
        lxError $ errWrongLen a


luaRawSet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
luaRawSet a b c = do
    case a of
        LTable _ table -> do
            case b of
                LNil -> lxError $ errNilIndex
                LDouble d | isNaN d -> lxError $ errNanIndex
                _ -> lxTableSet table b c
        _ -> lxError $ errWrongTable a


luaRead :: LuaRef q s a -> LuaState q s a
luaRead = lxRead


luaResume
    :: LuaThread q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
luaResume = lxResume


luaRunIO
    :: (forall q . LuaState q RealWorld t)
    -> IO (Either String t)
luaRunIO = do
    luaRunT
        (\st cont -> stToIO st >>= cont)
        (\io cont -> Just <$> io >>= cont)
        (return . Left)
        (return . Right)


luaRunST
    :: (forall q . LuaState q s t)
    -> ST s (Either String t)
luaRunST = do
    luaRunT
        (\st cont -> st >>= cont)
        (\_ cont -> cont Nothing)
        (return . Left)
        (return . Right)


luaRunT
    :: (forall u . ST s u -> (u -> r) -> r)
    -> (forall u . IO u -> (Maybe u -> r) -> r)
    -> (String -> r)
    -> (t -> r)
    -> (forall q . LuaState q s t)
    -> r
luaRunT onST onIO onError onPure act = do
    lxRunT
        onST
        onIO
        (\err -> case err of
            LString msg -> onError $ BSt.unpack msg
            _ -> onError $ "Unknown error")
        onPure $ do
            tr <- lxTry act
            case tr of
                Left err -> do
                    msg <- luaAsString err
                    lxError $ LString msg
                Right x -> return $ x


luaSet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
luaSet a b c = lxSet a b c


luaSetBoolMetatype
    :: LuaMetatype t
    => (Bool -> t q s)
    -> LuaState q s ()
luaSetBoolMetatype onBool = do
    lxSetBoolMetatable $
        LuaMetatypeWrapper (\f (LBool b) -> f $ onBool b)


luaSetFunctionMetatype
    :: LuaMetatype t
    => (LuaValue q s -> t q s)
    -> LuaState q s ()
luaSetFunctionMetatype onFunction = do
    lxSetFunctionMetatable $
        LuaMetatypeWrapper (\f x -> f $ onFunction x)


luaSetMetatable
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
luaSetMetatable a meta = do
    case a of
        LTable _ (LuaTable _ pmeta) -> do
            case meta of
                LTable _ _ -> lxWrite pmeta $ lxProduceMetatable meta
                LNil -> lxWrite pmeta $ lxDefaultMetatable
                _ -> lxError $ errWrongMetatableValue
        _ -> lxError $ errWrongMetatableOwner a


luaSetNilMetatype
    :: LuaMetatype t
    => t q s
    -> LuaState q s ()
luaSetNilMetatype onNil = do
    lxSetNilMetatable $
        LuaMetatypeWrapper (\f LNil -> f $ onNil)


luaSetNumberMetatype
    :: LuaMetatype t
    => (Integer -> t q s)
    -> (Rational -> t q s)
    -> (Double -> t q s)
    -> LuaState q s ()
luaSetNumberMetatype onInteger onRational onDouble = do
    lxSetNumberMetatable $
        LuaMetatypeWrapper (\f x -> do
            case x of
                LInteger i -> f $ onInteger i
                LRational q -> f $ onRational q
                LDouble d -> f $ onDouble d
                _ -> undefined)


luaSetStringMetatype
    :: LuaMetatype t
    => (BSt.ByteString -> t q s)
    -> LuaState q s ()
luaSetStringMetatype onString = do
    lxSetStringMetatable $
        LuaMetatypeWrapper (\f (LString s) -> f $ onString s)


luaSetTableMetatype
    :: LuaMetatype t
    => LuaValue q s
    -> (LuaValue q s -> t q s)
    -> LuaState q s ()
luaSetTableMetatype a onTable = do
    case a of
        LTable _ (LuaTable _ pmeta) -> do
            lxWrite pmeta $ LuaMetatypeWrapper (\f x -> f $ onTable x)
        _ -> lxError $ errWrongMetatableOwner a


luaSetThreadMetatype
    :: LuaMetatype t
    => (LuaValue q s -> t q s)
    -> LuaState q s ()
luaSetThreadMetatype onThread = do
    lxSetThreadMetatable $
        LuaMetatypeWrapper (\f x -> f $ onThread x)


luaSetWarnHandler
    :: (LuaValue q s -> LuaState q s ())
    -> LuaState q s ()
luaSetWarnHandler = lxSetWarnHandler


luaThreadClose
    :: LuaThread q s
    -> LuaState q s (Maybe (LuaValue q s))
luaThreadClose = lxThreadClose


luaThreadState
    :: LuaThread q s
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
luaThreadState = lxThreadState


luaToBoolean
    :: LuaValue q s
    -> Bool
luaToBoolean (LBool b) = b
luaToBoolean LNil = False
luaToBoolean _ = True


luaToDouble :: LuaValue q s -> Maybe Double
luaToDouble a = luaToNumber a
    Nothing
    (Just . fromInteger)
    (Just . fromRational)
    Just


luaToInteger :: LuaValue q s -> Maybe Integer
luaToInteger a = luaToNumber a
    Nothing
    Just
    (\q -> if denominator q == 1
        then Just $ numerator q
        else Nothing)
    (\d -> do
        if isInfinite d || isNaN d
            then Nothing
            else case toRational d of
                q | denominator q == 1 -> Just $ numerator q
                _ -> Nothing)


luaToFunction
    :: LuaValue q s
    -> Maybe (LuaFunction q s)
luaToFunction (LFunction _ f) = Just f
luaToFunction _ = Nothing


luaToNil
    :: LuaValue q s
    -> Maybe ()
luaToNil LNil = Just ()
luaToNil _ = Nothing


luaToNumber
    :: LuaValue q s
    -> a
    -> (Integer -> a)
    -> (Rational -> a)
    -> (Double -> a)
    -> a
luaToNumber a onOther onInteger onRational onDouble = do
    case a of
        LInteger i -> onInteger i
        LRational q -> onRational q
        LDouble d -> onDouble d
        LString s -> case luaLexNumber s of
            LInteger i -> onInteger i
            LRational q -> onRational q
            LDouble d -> onDouble d
            _ -> onOther
        _ -> onOther


luaToNumber2
    :: LuaValue q s
    -> LuaValue q s
    -> a
    -> a
    -> (Integer -> Integer -> a)
    -> (Rational -> Rational -> a)
    -> (Double -> Double -> a)
    -> a
luaToNumber2 a b onOtherA onOtherB onInteger2 onRational2 onDouble2 = do
    luaToNumber a
        onOtherA
        (\ia -> luaToNumber b
            onOtherB
            (\ib -> onInteger2 ia ib)
            (\qb -> onRational2 (fromInteger ia) qb)
            (\db -> onDouble2 (fromInteger ia) db))
        (\qa -> luaToNumber b
            onOtherB
            (\ib -> onRational2 qa (fromInteger ib))
            (\qb -> onRational2 qa qb)
            (\db -> onDouble2 (fromRational qa) db))
        (\da -> luaToNumber b
            onOtherB
            (\ib -> onDouble2 da (fromInteger ib))
            (\qb -> onDouble2 da (fromRational qb))
            (\db -> onDouble2 da db))


luaToRational :: LuaValue q s -> Maybe Rational
luaToRational a = luaToNumber a
    Nothing
    (Just . fromInteger)
    Just
    (\d -> do
        if isInfinite d || isNaN d
            then Nothing
            else Just $ toRational d)


luaToString
    :: LuaValue q s
    -> Maybe BSt.ByteString
luaToString x@(LInteger _) = Just $ BSt.pack $ show x
luaToString x@(LRational _) = Just $ BSt.pack $ show x
luaToString x@(LDouble _) = Just $ BSt.pack $ show x
luaToString (LString b) = Just $ b
luaToString _ = Nothing


luaToThread
    :: LuaValue q s
    -> Maybe (LuaThread q s)
luaToThread (LThread _ th) = Just th
luaToThread _ = Nothing


luaToUserdata
    :: (Typeable u)
    => LuaValue q s
    -> Maybe (u q s)
luaToUserdata a = do
    case a of
        LUserdata _ x -> hcast x
        _ -> hcast a
    where
    hcastWith :: a q s -> (a :~~: b) -> b q s
    hcastWith x HRefl = x
    hcast :: (Typeable a, Typeable b) => a q s -> Maybe (b q s)
    hcast x = hcastWith x <$> eqTypeRep typeRep typeRep


luaTry
    :: LuaState q s t
    -> LuaState q s (Either (LuaValue q s) t)
luaTry = lxTry


luaTypename
    :: LuaValue q s
    -> BSt.ByteString
luaTypename = lvalTypename


luaUncons
    :: [LuaValue q s]
    -> (LuaValue q s, [LuaValue q s])
luaUncons xs = case xs of
    [] -> (LNil, [])
    a:r -> (a, r)


luaWarn
    :: LuaValue q s
    -> LuaState q s ()
luaWarn = lxWarn


luaWithErrHandler
    :: LuaErrorHandler q s
    -> LuaState q s t
    -> LuaState q s t
luaWithErrHandler errh act = do
    lxWithErrHandler errh act


luaWrite :: LuaRef q s a -> a -> LuaState q s ()
luaWrite = lxWrite


luaXPCall
    :: LuaValue q s
    -> LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
luaXPCall ha a args = luaXTry errh $ lxCall a args
    where
    errh e = lxCar <$> lxCall ha [e]


luaXTry
    :: LuaErrorHandler q s
    -> LuaState q s t
    -> LuaState q s (Either (LuaValue q s) t)
luaXTry errh act = do
    lxWithErrHandler errh $ lxTry act


luaYield
    :: [LuaValue q s]
    -> LuaState q s [LuaValue q s]
luaYield = lxYield


{-# COMPLETE (:|) #-}
infixr 5 :|
pattern (:|) :: LuaValue q s -> [LuaValue q s] -> [LuaValue q s]
pattern (:|) a r <- (luaUncons -> (a, r))
    where
    a :| r = a:r
