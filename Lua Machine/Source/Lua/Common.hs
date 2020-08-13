{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}


module Lua.Common (
    LuaErrorHandler,
    LuaFunction,
    LuaIterator,
    LuaMetatype(..),
    LuaMetatypeWrapper(..),
    LuaPure(..),
    LuaPush(..),
    LuaRef,
    LuaState,
    LuaValue(..),
    errArgRange,
    errArgType,
    errDivideZero,
    errNilIndex,
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
    luaCreatePureUserdata,
    luaCreateTable,
    luaCreateThread,
    luaCreateUserdata,
    luaError,
    luaExtend,
    luaFromUserdata,
    luaGet,
    luaGetMetatable,
    luaIsNumber,
    luaLen,
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
    luaThreadState,
    luaToBoolean,
    luaToDouble,
    luaToInteger,
    luaToFunction,
    luaToNil,
    luaToPureUserdata,
    luaToRational,
    luaToString,
    luaToUserdata,
    luaTry,
    luaTypename,
    luaWithErrHandler,
    luaWrite,
    luaXPCall,
    luaXTry,
    luaYield,
) where


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
    lxMetatable a (\x -> lxFinally (lmtClose x) act)


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
    lxRawCompareEq lmtLe a b $
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


luaCreatePureUserdata
    :: LuaMetatype (LuaPure t)
    => t
    -> LuaState q s (LuaValue q s)
luaCreatePureUserdata px = luaCreateUserdata (LuaPure px)


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
    i <- lxNewId
    th <- lxNewThread f
    return $ LThread i th


luaCreateUserdata
    :: LuaMetatype t
    => t q s
    -> LuaState q s (LuaValue q s)
luaCreateUserdata x = do
    i <- lxNewId
    return $ LUserdata i x


luaError :: LuaValue q s -> LuaState q s a
luaError e = lxError e


luaExtend :: Int -> [LuaValue q s] -> [LuaValue q s]
luaExtend n as = lxExtend n as


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
    case (a, c) of
        (LInteger start, LInteger step) -> do
            ratlimit <- maybe err return $ luaToRational b
            let limit = if step > 0
                    then floor ratlimit
                    else ceiling ratlimit
            iterFunc start limit step
        _ -> do
            start <- maybe err return $ luaToRational a
            limit <- maybe err return $ luaToRational b
            step <- maybe err return $ luaToRational c
            iterFunc start limit step
    where
    err = lxError $ errWrongRangeIter
    iterCond limit step
        | step == 0 = lxError $ errZeroStep
        | step > 0 = return $ (<= limit)
        | otherwise = return $ (>= limit)
    iterFunc start limit step = do
        cond <- iterCond limit step
        pref <- lxAlloc start
        let func _ = do
            param <- lxRead pref
            if cond param
                then do
                    lxWrite pref (param + step)
                    return $ [luaPush param]
                else return $ [LNil]
        luaCreateFunction func


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
                _ -> lxTableSet table b c
        _ -> lxError $ errWrongTable a


luaRead :: LuaRef q s a -> LuaState q s a
luaRead = lxRead


luaResume
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
luaResume a args = do
    case a of
        LThread _ thread -> lxResume thread (Right args)
        _ -> lxError $ errWrongResume a


luaRunIO
    :: (forall q . LuaState q RealWorld t)
    -> IO (Either String t)
luaRunIO = luaRunT resolveST resolveIO resolveYield onError onPure
    where
    resolveST act = stToIO $ act
    resolveIO act = Just <$> act
    resolveYield _ = return $ Left $ errWrongYield
    onError (LString msg)
        = return $ Left $ BSt.unpack msg
    onError _ = return $ Left $ "Unknown error"
    onPure x = return $ Right $ x


luaRunST
    :: (forall q . LuaState q s t)
    -> ST s (Either String t)
luaRunST = luaRunT resolveST resolveIO resolveYield onError onPure
    where
    resolveST act = act
    resolveIO _ = return $ Nothing
    resolveYield _ = return $ Left $ errWrongYield
    onError (LString msg)
        = return $ Left $ BSt.unpack msg
    onError _ = return $ Left $ "Unknown error"
    onPure x = return $ Right $ x


luaRunT
    :: (Monad m)
    => (forall a . ST s a -> m a)
    -> (forall a . IO a -> m (Maybe a))
    -> (forall q . [LuaValue q s] -> m (Either (LuaValue q s) [LuaValue q s]))
    -> (forall q . LuaValue q s -> m u)
    -> (t -> m u)
    -> (forall q . LuaState q s t)
    -> m u
luaRunT resolveST resolveIO resolveYield onError onPure act = do
    lxRunT resolveST resolveIO resolveYield onError onPure (do
        tr <- lxTry act
        case tr of
            Left err -> do
                msg <- luaAsString err
                luaError $ LString msg
            Right x -> return $ x)


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


luaThreadState
    :: LuaValue q s
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
luaThreadState a onRunning onSuspended onNormal onDead = do
    case a of
        LThread _ th -> lxThreadState th onRunning onSuspended onNormal onDead
        _ -> luaError $ errWrongThreadStatus a


luaToBoolean
    :: LuaValue q s
    -> Bool
luaToBoolean (LBool b) = b
luaToBoolean LNil = False
luaToBoolean _ = True


luaToDouble :: LuaValue q s -> Maybe Double
luaToDouble (LInteger x) = Just $ fromInteger x
luaToDouble (LRational x) = Just $ fromRational x
luaToDouble (LDouble x) = Just $ x
luaToDouble _ = Nothing


luaToInteger :: LuaValue q s -> Maybe Integer
luaToInteger (LInteger x) = Just x
luaToInteger (LRational x) =
    if denominator x == 1
        then Just $ numerator x
        else Nothing
luaToInteger (LDouble x) =
    if isNaN x || isInfinite x
        then Nothing
        else luaToInteger $ LRational $ toRational x
luaToInteger _ = Nothing


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


luaToPureUserdata
    :: (Typeable u)
    => LuaValue q s
    -> Maybe u
luaToPureUserdata a = do
    case luaToUserdata a of
        Just (LuaPure px) -> px
        Nothing -> Nothing


luaToRational :: LuaValue q s -> Maybe Rational
luaToRational (LInteger x) = Just $ fromInteger x
luaToRational (LRational x) = Just $ x
luaToRational (LDouble x) =
    if isNaN x || isInfinite x
        then Nothing
        else Just $ toRational x
luaToRational _ = Nothing


luaToString
    :: LuaValue q s
    -> Maybe BSt.ByteString
luaToString x@(LInteger _) = Just $ BSt.pack $ show x
luaToString x@(LRational _) = Just $ BSt.pack $ show x
luaToString x@(LDouble _) = Just $ BSt.pack $ show x
luaToString (LString b) = Just $ b
luaToString _ = Nothing


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
