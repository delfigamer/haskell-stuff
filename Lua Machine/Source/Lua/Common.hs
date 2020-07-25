{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}


module Lua.Common (
    LuaErrorHandler,
    LuaFunction,
    LuaMetatable(..),
    LuaPure(..),
    LuaPush(..),
    LuaRef,
    LuaState,
    LuaValue(..),
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
    luaCall,
    luaCloseAfter,
    luaCompareLt,
    luaCompareLe,
    luaCompareEq,
    luaConcat,
    luaCreateFunction,
    luaCreatePureUserdata,
    luaCreateTable,
    luaCreateThread,
    luaCreateUserdata,
    luaError,
    luaExtend,
    luaFromUserdata,
    luaGet,
    luaGetMetatable,
    luaLen,
    luaLiftIO,
    luaLiftST,
    luaNewTable,
    luaNewThread,
    luaPCall,
    luaRangeIter,
    luaRawEqual,
    luaRawGet,
    luaRawLen,
    luaRawSet,
    luaRead,
    luaResume,
    luaRunT,
    luaSet,
    luaSetMetatable,
    luaToBoolean,
    luaToDouble,
    luaToInteger,
    luaToFunction,
    luaToPureUserdata,
    luaToRational,
    luaToString,
    luaToUserdata,
    luaTry,
    luaWrite,
    luaXPCall,
    luaXTry,
    luaYield,
) where


import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Maybe
import Type.Reflection
import qualified Data.ByteString.Char8 as BSt
import Data.Ratio
import Lua.Core


luaAlloc :: a -> LuaState q s (LuaRef q s a)
luaAlloc = lxAlloc


luaArithUnm a = lxPerformUnary
    lxRawArithUnm
    lmtUnm
    (lxError $ errWrongArith1 a)
    a

luaArithBNot a = lxPerformUnary
    lxRawArithBNot
    lmtBNot
    (lxError $ errWrongArith1 a)
    a

luaArithAdd a b = lxPerformBinary
    (lxRawArithNum (+))
    lmtAdd
    (lxError $ errWrongArith2 a b)
    a b

luaArithSub a b = lxPerformBinary
    (lxRawArithNum (-))
    lmtSub
    (lxError $ errWrongArith2 a b)
    a b

luaArithMul a b = lxPerformBinary
    (lxRawArithNum (*))
    lmtMul
    (lxError $ errWrongArith2 a b)
    a b

luaArithDiv a b = lxPerformBinary
    lxRawArithDiv
    lmtDiv
    (lxError $ errWrongArith2 a b)
    a b

luaArithIDiv a b = lxPerformBinary
    lxRawArithIDiv
    lmtIDiv
    (lxError $ errWrongArith2 a b)
    a b

luaArithMod a b = lxPerformBinary
    lxRawArithMod
    lmtMod
    (lxError $ errWrongArith2 a b)
    a b

luaArithPow a b = lxPerformBinary
    lxRawArithPow
    lmtPow
    (lxError $ errWrongArith2 a b)
    a b

luaArithBAnd a b = lxPerformBinary
    (lxRawArithBitwise (.&.))
    lmtBAnd
    (lxError $ errWrongArith2 a b)
    a b

luaArithBOr a b = lxPerformBinary
    (lxRawArithBitwise (.|.))
    lmtBOr
    (lxError $ errWrongArith2 a b)
    a b

luaArithBXor a b = lxPerformBinary
    (lxRawArithBitwise xor)
    lmtBXor
    (lxError $ errWrongArith2 a b)
    a b

luaArithShl a b = lxPerformBinary
    (lxRawArithBitwise (\x y -> shiftL x (fromInteger y)))
    lmtShl
    (lxError $ errWrongArith2 a b)
    a b

luaArithShr a b = lxPerformBinary
    (lxRawArithBitwise (\x y -> shiftR x (fromInteger y)))
    lmtShr
    (lxError $ errWrongArith2 a b)
    a b


luaCall
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s [LuaValue q s]
luaCall a args = lxCall a args


luaCloseAfter
    :: LuaValue q s
    -> LuaState q s t
    -> LuaState q s t
luaCloseAfter a act = do
    lxMetatable a (\x mt ->
        case lmtClose mt of
            Just metaop -> lxFinally (metaop x) act
            Nothing -> act)


luaCompareLt a b = lxPerformBinary
    (lxRawCompare (<))
    lmtLt
    (lxError $ errWrongCompare a b)
    a b


luaCompareLe a b = lxPerformBinary
    (lxRawCompare (<=))
    lmtLe
    (lxError $ errWrongCompare a b)
    a b


luaCompareEq a b = lxPerformBinary
    lxRawCompareEq
    lmtLe
    (return $ False)
    a b


luaConcat a b = lxPerformBinary
    lxRawConcat
    lmtConcat
    (lxError $ errWrongConcat a b)
    a b


luaCreateFunction
    :: LuaFunction q s
    -> LuaState q s (LuaValue q s)
luaCreateFunction fn = do
    cnt <- lxNewId
    return $ LFunction cnt fn


luaCreatePureUserdata
    :: t
    -> LuaMetatable q s (LuaPure t)
    -> LuaState q s (LuaValue q s)
luaCreatePureUserdata px mt = luaCreateUserdata (LuaPure px) mt


luaCreateTable
    :: [(LuaValue q s, LuaValue q s)]
    -> LuaState q s (LuaValue q s)
luaCreateTable init = do
    kinit <- forM init (\(kv, value) -> do
        key <- lxKey kv
        return $ (key, value))
    i <- lxNewId
    table <- lxAllocTable kinit
    return $ LTable i table


luaCreateThread
    :: LuaFunction q s
    -> LuaState q s (LuaValue q s)
luaCreateThread f = LThread <$> lxNewId <*> lxNewThread f


luaCreateUserdata
    :: t q s
    -> LuaMetatable q s t
    -> LuaState q s (LuaValue q s)
luaCreateUserdata x mt = do
    i <- lxNewId
    return $ LUserdata i x mt


luaError :: LuaValue q s -> LuaState q s a
luaError e = lxError e


luaExtend :: Int -> [LuaValue q s] -> [LuaValue q s]
luaExtend n as = lxExtend n as


luaFromUserdata
    :: LuaValue q s
    -> (forall t . (Typeable t) => t q s -> LuaState q s u)
    -> LuaState q s u
luaFromUserdata a f = do
    lxMetatable a (\x mt -> f x)


luaGet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
luaGet a b = lxGet a b


luaGetMetatable
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
luaGetMetatable a = do
    lxMetatable a (\x mt ->
        return $ lmtMetatable mt)


luaLen
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
luaLen a = do
    case a of
        LString x -> return $ LInteger $ toInteger (BSt.length x)
        _ -> lxMetatable a (\x mt ->
            case lmtLen mt of
                Just metaop -> metaop x
                Nothing ->
                    case a of
                        LTable _ table -> LInteger <$> lxTableLength table
                        _ -> lxError $ errWrongLen a)


luaLiftIO
    :: IO t
    -> LuaState q s t
luaLiftIO act = do
    mr <- lxTryLiftIO act
    case mr of
        Nothing -> lxError $ errWrongIO
        Just r -> return $ r


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
luaNewThread = luaCreateThread (\init -> do
    case init of
        (LFunction _ f):args -> f args
        a:args -> lxError $ errWrongThreadFunc a
        _ -> lxError $ errWrongThreadFunc LNil)


luaPCall
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
luaPCall a args = lxTry $ luaCall a args


class LuaPush a where
    luaPush :: a -> LuaValue q s

instance LuaPush () where
    luaPush x = LNil

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
        (LInteger init, LInteger step) -> do
            ratlimit <- maybe err return $ luaToRational b
            let limit = if step > 0
                    then floor ratlimit
                    else ceiling ratlimit
            iterFunc init limit step
        _ -> do
            init <- maybe err return $ luaToRational a
            limit <- maybe err return $ luaToRational b
            step <- maybe err return $ luaToRational c
            iterFunc init limit step
    where
    err = lxError $ errWrongRangeIter
    iterCond limit step
        | step == 0 = lxError $ errZeroStep
        | step > 0 = return $ (<= limit)
        | step < 0 = return $ (>= limit)
    iterFunc init limit step = do
        cond <- iterCond limit step
        pref <- lxAlloc init
        let func args = do
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
    fromMaybe False <$> lxRawCompareEq a b


luaRawGet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
luaRawGet a b = do
    case a of
        LTable _ table -> do
            case b of
                LNil -> return $ LNil
                _ -> do
                    key <- lxKey b
                    result <- lxGetTable key table
                    case result of
                        Just ret -> return $ ret
                        Nothing -> return $ LNil
        _ -> lxError $ errWrongTable a


luaRawLen
    :: LuaValue q s
    -> LuaState q s Integer
luaRawLen a = do
    rr <- lxRawLen a
    case rr of
        Just len -> return $ len
        Nothing -> lxError $ errWrongLen a


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
                _ -> do
                    key <- lxKey b
                    lxSetTable key c table
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


luaRunT
    :: (Monad m)
    => (forall a . ST s a -> m a)
    -> (forall a . IO a -> m (Maybe a))
    -> (forall q . [LuaValue q s] -> m (Either (LuaValue q s) [LuaValue q s]))
    -> (forall q . LuaValue q s -> m u)
    -> (t -> m u)
    -> (forall q . LuaState q s t)
    -> m u
luaRunT = lxRunT


luaSet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
luaSet a b c = lxSet a b c


luaSetMetatable
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
luaSetMetatable a meta = do
    case a of
        LTable _ table -> do
            case meta of
                LTable _ _ -> do
                    LuaTableBody items _ <- lxRead table
                    mt <- lxProduceMetatable meta
                    lxWrite table $ LuaTableBody items mt
                _ -> lxError $ errWrongMetatableValue
        _ -> lxError $ errWrongMetatableOwner a


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
    -> Maybe String
luaToString x@(LInteger _) = Just $ show x
luaToString x@(LRational _) = Just $ show x
luaToString x@(LDouble _) = Just $ show x
luaToString (LString b) = Just $ BSt.unpack b
luaToString _ = Nothing


luaToUserdata
    :: (Typeable u)
    => LuaValue q s
    -> Maybe (u q s)
luaToUserdata a = do
    case a of
        LUserdata _ x mt -> lmtTypify mt hcast x
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


luaWrite :: LuaRef q s a -> a -> LuaState q s ()
luaWrite = lxWrite


luaXPCall
    :: LuaValue q s
    -> LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
luaXPCall ha a args = luaXTry errh $ luaCall a args
    where
    errh e = lxCar <$> luaCall e [a]


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
