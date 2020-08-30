{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}


module Lua.Core (
    LuaErrorHandler,
    LuaFunction,
    LuaIterator,
    LuaMetatype(..),
    LuaMetatypeWrapper(..),
    LuaRef,
    LuaStackFrame(..),
    LuaState,
    LuaTable(..),
    LuaThread,
    LuaValue(..),
    LuaVariableList,
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
    lsfModifyLocals,
    lvalTypename,
    lxAlloc,
    lxAllocTable,
    lxAsString,
    lxAskStack,
    lxCall,
    lxCar,
    lxCurrentThread,
    lxDefaultMetatable,
    lxError,
    lxErrorAt,
    lxExtend,
    lxFinally,
    lxGet,
    lxGetDebugHook,
    lxIsYieldable,
    lxLiftST,
    lxLocalStack,
    lxMetatable,
    lxNewId,
    lxNewThread,
    lxPerformBinary,
    lxPerformMetaopBinary,
    lxPerformMetaopUnary,
    lxPerformUnary,
    lxProduceMetatable,
    lxRawArithBNot,
    lxRawArithBitwise,
    lxRawArithDiv,
    lxRawArithIDiv,
    lxRawArithMod,
    lxRawArithNum,
    lxRawArithPow,
    lxRawArithUnm,
    lxRawCompare,
    lxRawCompareEq,
    lxRawLen,
    lxRead,
    lxRegistryGet,
    lxRegistrySet,
    lxResume,
    lxRunHook,
    lxRunT,
    lxSet,
    lxSetBoolMetatable,
    lxSetDebugHook,
    lxSetFunctionMetatable,
    lxSetNilMetatable,
    lxSetNumberMetatable,
    lxSetStackLimit,
    lxSetStringMetatable,
    lxSetThreadMetatable,
    lxSetWarnHandler,
    lxStackLevel,
    lxTableGet,
    lxTableIter,
    lxTableLength,
    lxTableNext,
    lxTableSet,
    lxThreadClose,
    lxThreadState,
    lxToNumber,
    lxToNumber2,
    lxTry,
    lxTryLiftIO,
    lxWarn,
    lxWithErrHandler,
    lxWrite,
    lxYield,
) where


import Control.Monad.ST
import Data.Bits
import Data.Function
import Data.Maybe
import Data.Ratio
import Data.STRef
import Data.Typeable (Proxy(..))
import Type.Reflection
import qualified Data.ByteString.Char8 as BSt
import qualified Data.Map.Strict as M
import Lua.SourceRange
import qualified Lua.Coroutine as Y
import qualified Lua.Table as T


errArgRange
    :: Int
    -> LuaValue q s
errArgRange n = LString $ BSt.concat [
        "Argument ", BSt.pack (show n), " is out of range"]

errArgType
    :: Int
    -> BSt.ByteString
    -> LuaValue q s
    -> LuaValue q s
errArgType n expected value = LString $ BSt.concat [
        "Expected a ", expected, " at argument ",
        BSt.pack (show n),
        ", got a ", lvalTypename value]

errDivideZero :: LuaValue q s
errDivideZero = LString $ "Divide by zero"

errLenType :: LuaValue q s
errLenType = LString "Object length is not an integer"

errNanIndex :: LuaValue q s
errNanIndex = LString $ "Attempt to index a table with a NaN"

errNilIndex :: LuaValue q s
errNilIndex = LString $ "Attempt to index a table with a nil"

errNoArg
    :: Int
    -> BSt.ByteString
    -> LuaValue q s
errNoArg n expected = LString $ BSt.concat [
        "Expected a ", expected, " at argument ",
        BSt.pack (show n),
        ", got nothing"]

errNonSuspended :: LuaValue q s
errNonSuspended = LString $ "Attempt to resume a non-suspended coroutine"

errStack :: LuaValue q s
errStack = LString $ "Stack overflow"

errWrongArith1 :: LuaValue q s -> LuaValue q s
errWrongArith1 a
    = LString $ BSt.concat [
        "Attempt to perform arithmetic on a ", lvalTypename a]

errWrongArith2 :: LuaValue q s -> LuaValue q s -> LuaValue q s
errWrongArith2 a b
    = LString $ BSt.concat [
        "Attempt to perform arithmetic between a ", lvalTypename a,
        " and a ", lvalTypename b]

errWrongBit :: LuaValue q s
errWrongBit = LString $
    "Attempt to perform bitwise on a non-integer number"

errWrongCall :: LuaValue q s -> LuaValue q s
errWrongCall a = LString $ BSt.concat [
    "Attempt to call a ", lvalTypename a]

errWrongCompare :: LuaValue q s -> LuaValue q s -> LuaValue q s
errWrongCompare a b
    = LString $ BSt.concat [
        "Attempt to compare a ", lvalTypename a, " and a ", lvalTypename b]

errWrongConcat :: LuaValue q s -> LuaValue q s -> LuaValue q s
errWrongConcat a b
    = LString $ BSt.concat [
        "Attempt to concatenate a ", lvalTypename a, " and a ", lvalTypename b]

errWrongIO :: LuaValue q s
errWrongIO = LString $
    "Attempt to perform IO where it's unsupported"

errWrongLen :: LuaValue q s -> LuaValue q s
errWrongLen a
    = LString $ BSt.concat [
        "Attempt to take length of a ", lvalTypename a]

errWrongResume :: LuaValue q s -> LuaValue q s
errWrongResume a
    = LString $ BSt.concat [
        "Attempt to resume a ", lvalTypename a]

errWrongMetatableOwner :: LuaValue q s -> LuaValue q s
errWrongMetatableOwner a
    = LString $ BSt.concat [
        "Attempt to set a metatable for a ", lvalTypename a]

errWrongMetatableValue :: LuaValue q s
errWrongMetatableValue = LString $
    "Attempt to set a non-table value as a metatable"

errWrongRangeIter :: LuaValue q s
errWrongRangeIter= LString $
    "Attempt to perform a non-numeric ranged for loop"

errWrongTable :: LuaValue q s -> LuaValue q s
errWrongTable a
    = LString $ BSt.concat [
        "Attempt to index a ", lvalTypename a]

errWrongThreadClose :: LuaValue q s
errWrongThreadClose
    = LString $ BSt.concat [
        "Attempt to close an active coroutine"]

errWrongThreadFunc :: LuaValue q s -> LuaValue q s
errWrongThreadFunc a
    = LString $ BSt.concat [
        "Attempt to create a coroutine from ", lvalTypename a]

errWrongThreadStatus :: LuaValue q s -> LuaValue q s
errWrongThreadStatus a
    = LString $ BSt.concat [
        "Attempt to get a coroutine status from ", lvalTypename a]

errWrongYield :: LuaValue q s
errWrongYield = LString "Attempt to yield in a non-yieldable context"

errZeroStep :: LuaValue q s
errZeroStep = LString "Attempt to perform a ranged for loop with step zero"


type LuaMetaopUnary r q s t
    =  t q s
    -> LuaState q s r
    -> LuaState q s r


type LuaMetaopBinary r q s t
    =  t q s
    -> LuaValue q s
    -> Bool
    -> LuaState q s r
    -> LuaState q s r


class Typeable t => LuaMetatype t where
    lmtMetatable :: t q s -> LuaValue q s
    lmtTypename :: t q s -> Maybe BSt.ByteString
    lmtAdd :: LuaMetaopBinary (LuaValue q s) q s t
    lmtSub :: LuaMetaopBinary (LuaValue q s) q s t
    lmtMul :: LuaMetaopBinary (LuaValue q s) q s t
    lmtDiv :: LuaMetaopBinary (LuaValue q s) q s t
    lmtMod :: LuaMetaopBinary (LuaValue q s) q s t
    lmtPow :: LuaMetaopBinary (LuaValue q s) q s t
    lmtUnm :: LuaMetaopUnary (LuaValue q s) q s t
    lmtIDiv :: LuaMetaopBinary (LuaValue q s) q s t
    lmtBAnd :: LuaMetaopBinary (LuaValue q s) q s t
    lmtBOr :: LuaMetaopBinary (LuaValue q s) q s t
    lmtBXor :: LuaMetaopBinary (LuaValue q s) q s t
    lmtBNot :: LuaMetaopUnary (LuaValue q s) q s t
    lmtShl :: LuaMetaopBinary (LuaValue q s) q s t
    lmtShr :: LuaMetaopBinary (LuaValue q s) q s t
    lmtConcat :: LuaMetaopBinary (LuaValue q s) q s t
    lmtLen :: LuaMetaopUnary (LuaValue q s) q s t
    lmtEq :: LuaMetaopBinary Bool q s t
    lmtLt :: LuaMetaopBinary Bool q s t
    lmtLe :: LuaMetaopBinary Bool q s t
    lmtIndex
        :: t q s
        -> LuaValue q s
        -> LuaState q s (LuaValue q s)
        -> LuaState q s (LuaValue q s)
    lmtNewIndex
        :: t q s
        -> LuaValue q s
        -> LuaValue q s
        -> LuaState q s ()
        -> LuaState q s ()
    lmtCall
        :: t q s
        -> [LuaValue q s]
        -> LuaState q s [LuaValue q s]
        -> LuaState q s [LuaValue q s]
    lmtClose :: t q s -> LuaValue q s -> LuaState q s ()
    lmtAsString
        :: t q s
        -> LuaState q s BSt.ByteString
        -> LuaState q s BSt.ByteString
    lmtShow
        :: t q s -> String -> String
    lmtMetatable _self = LNil
    lmtTypename _self = Nothing
    lmtAdd _self _other _rev def = def
    lmtSub _self _other _rev def = def
    lmtMul _self _other _rev def = def
    lmtDiv _self _other _rev def = def
    lmtMod _self _other _rev def = def
    lmtPow _self _other _rev def = def
    lmtUnm _self def = def
    lmtIDiv _self _other _rev def = def
    lmtBAnd _self _other _rev def = def
    lmtBOr _self _other _rev def = def
    lmtBXor _self _other _rev def = def
    lmtBNot _self def = def
    lmtShl _self _other _rev def = def
    lmtShr _self _other _rev def = def
    lmtConcat _self _other _rev def = def
    lmtLen _self def = def
    lmtEq _self _other _rev def = def
    lmtLt _self _other _rev def = def
    lmtLe _self _other _rev def = def
    lmtIndex _self _index def = def
    lmtNewIndex _self _index _value def = def
    lmtCall _self _args def = def
    lmtClose _self _err = return ()
    lmtAsString _self def = def
    lmtShow _self def = def


data LuaAnnotated q s = LuaAnnotated
    !LuaId
    !(LuaTable q s)
    !(LuaValue q s)


iimtParse
    :: BSt.ByteString
    -> LuaAnnotated q s
    -> LuaState q s (LuaValue q s, LuaValue q s)
iimtParse !metaname (LuaAnnotated _ (LuaTable tbody _) value) = do
    (_, !metaop) <- lxLiftST $ T.get tbody (T.KString metaname)
    return $ (value, metaop)


iimtMetaopUnary
    :: BSt.ByteString
    -> LuaAnnotated q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
iimtMetaopUnary !metaname !self def = do
    (value, metaop) <- iimtParse metaname self
    case metaop of
        LNil -> def
        f -> lxCar <$> lxCall f [value, value]


iimtMetaopBinary
    :: BSt.ByteString
    -> LuaAnnotated q s
    -> LuaValue q s
    -> Bool
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
iimtMetaopBinary !metaname !self !other !rev def = do
    (value, metaop) <- iimtParse metaname self
    case metaop of
        LNil -> def
        f -> case rev of
            False -> lxCar <$> lxCall f [value, other]
            True -> lxCar <$> lxCall f [other, value]


iimtMetaopBinaryBool
    :: BSt.ByteString
    -> LuaAnnotated q s
    -> LuaValue q s
    -> Bool
    -> LuaState q s Bool
    -> LuaState q s Bool
iimtMetaopBinaryBool !metaname !self !other !rev def = do
    (value, metaop) <- iimtParse metaname self
    case metaop of
        LNil -> def
        f -> case rev of
            False -> listToBool <$> lxCall f [value, other]
            True -> listToBool <$> lxCall f [other, value]
    where
    listToBool [] = False
    listToBool (LNil:_) = False
    listToBool (LBool False:_) = False
    listToBool _ = True


instance LuaMetatype LuaAnnotated where
    lmtMetatable (LuaAnnotated lid table _) = LTable lid table
    lmtAdd = iimtMetaopBinary "__add"
    lmtSub = iimtMetaopBinary "__sub"
    lmtMul = iimtMetaopBinary "__mul"
    lmtDiv = iimtMetaopBinary "__div"
    lmtMod = iimtMetaopBinary "__mod"
    lmtPow = iimtMetaopBinary "__pow"
    lmtUnm = iimtMetaopUnary "__unm"
    lmtIDiv = iimtMetaopBinary "__idiv"
    lmtBAnd = iimtMetaopBinary "__band"
    lmtBOr = iimtMetaopBinary "__bor"
    lmtBXor = iimtMetaopBinary "__bxor"
    lmtBNot = iimtMetaopUnary "__bnot"
    lmtShl = iimtMetaopBinary "__shl"
    lmtShr = iimtMetaopBinary "__shr"
    lmtConcat = iimtMetaopBinary "__concat"
    lmtLen = iimtMetaopUnary "__len"
    lmtEq = iimtMetaopBinaryBool "__eq"
    lmtLt = iimtMetaopBinaryBool "__lt"
    lmtLe = iimtMetaopBinaryBool "__le"
    lmtIndex !self !index def = do
        (value, metaop) <- iimtParse "__index" self
        case metaop of
            LNil -> def
            LFunction _ ff -> lxCar <$> ff [value, index]
            next -> lxStackLevel $ lxGet next index
    lmtNewIndex !self !index !new def = do
        (value, metaop) <- iimtParse "__newindex" self
        case metaop of
            LNil -> def
            LFunction _ ff -> () <$ ff [value, index, new]
            next -> lxStackLevel $ lxSet next index new
    lmtCall !self args def = do
        (value, metaop) <- iimtParse "__call" self
        case metaop of
            LNil -> def
            next -> lxCall next (value:args)
    lmtClose !self err = do
        (value, metaop) <- iimtParse "__close" self
        case metaop of
            LNil -> return ()
            f -> () <$ lxCall f [value, err]
    lmtAsString !self def = do
        (value, metaop) <- iimtParse "__tostring" self
        case metaop of
            LNil -> def
            LFunction _ ff -> lxStackLevel . lxAsString . lxCar =<< ff [value]
            next -> lxStackLevel $ lxAsString next


newtype LuaUnannotated q s = LuaUnannotated (LuaValue q s)


instance LuaMetatype LuaUnannotated


newtype LuaId = LuaId Int deriving (Show, Eq, Ord)


data LuaTable q s = LuaTable {
    ltbBody :: T.Table s (LuaValue q s, LuaValue q s),
    ltbMetatable :: LuaRef q s (LuaMetatypeWrapper q s)}


instance T.Nilable (LuaValue q s, LuaValue q s) where
    nil = (LNil, LNil)
    isNil (_, LNil) = True
    isNil _ = False


type LuaFunction q s = [LuaValue q s] -> LuaState q s [LuaValue q s]


newtype LuaCloseChain q s =
    LuaCloseChain
        (  Maybe (LuaValue q s)
        -> LuaCoroutine q s (Maybe (LuaValue q s)))


instance Semigroup (LuaCloseChain q s) where
    LuaCloseChain a <> LuaCloseChain b = LuaCloseChain $ \x -> a x >>= b


instance Monoid (LuaCloseChain q s) where
    mempty = LuaCloseChain $ pure


type LuaCoroutine q s =
    Y.Coroutine
        [LuaValue q s]
        (LuaCloseChain q s, [LuaValue q s])
        (LuaValue q s)
        q
        (LuaLifted s)


type LuaSuspend q s =
    Y.Suspend
        [LuaValue q s]
        (LuaCloseChain q s, [LuaValue q s])
        (LuaValue q s)
        q


data LuaThreadState q s
    = LTRunning !Bool
    | LTSuspended
        (LuaCloseChain q s)
        (LuaSuspend q s)
    | LTNormal !Bool
    | LTDead


newtype LuaThread q s = LuaThread (LuaRef q s (LuaThreadState q s))


data LuaValue q s where
    LNil :: LuaValue q s
    LBool :: !Bool -> LuaValue q s
    LInteger :: !Integer -> LuaValue q s
    LRational :: !Rational -> LuaValue q s
    LDouble :: !Double -> LuaValue q s
    LString :: !BSt.ByteString -> LuaValue q s
    LFunction
        :: !LuaId
        -> LuaFunction q s
        -> LuaValue q s
    LThread
        :: !LuaId
        -> LuaThread q s
        -> LuaValue q s
    LTable
        :: !LuaId
        -> LuaTable q s
        -> LuaValue q s
    LUserdata
        :: (LuaMetatype t)
        => !LuaId
        -> !(t q s)
        -> LuaValue q s


showRational :: Rational -> String
showRational r = do
    case rationalToDecimal (numerator r) (denominator r) 0 of
        Nothing -> show (numerator r) ++ "/" ++ show (denominator r)
        Just (x, dexp)
            | x < 0 -> '-':showDecAbs (-x) dexp
            | otherwise -> showDecAbs x dexp
    where
    showDecAbs x dexp = do
        case ilog10 x dexp of
            xlog
                | xlog < -4 -> showDecAbsExp x dexp xlog
                | xlog < 14 && dexp < 0 -> dtos x (1-dexp) (-dexp) ""
                | xlog < 14 -> dtos (x * 10^(dexp+1)) 2 1 ""
                | otherwise -> showDecAbsExp x dexp xlog
    showDecAbsExp x dexp xlog = do
        dtos x (1-dexp+xlog) (-dexp+xlog) xlogstr
        where
        xlogstr
            | xlog < 0 = 'e':'-':itos (-xlog) 2 ""
            | otherwise = 'e':'+':itos xlog 2 ""
    rationalToDecimal :: Integer -> Integer -> Int -> Maybe (Integer, Int)
    rationalToDecimal num den dexp
        | den <= 0 = Nothing
        | den == 1 = do
            case num `divMod` 10 of
                (0, 0) -> Just (num, dexp)
                (numq, 0) -> rationalToDecimal numq den (dexp+1)
                _ -> Just (num, dexp)
        | let (denq, denr) = den `divMod` 10, denr == 0 = do
            rationalToDecimal num denq (dexp-1)
        | let (denq, denr) = den `divMod` 5, denr == 0 = do
            rationalToDecimal (num*2) denq (dexp-1)
        | let (denq, denr) = den `divMod` 2, denr == 0 = do
            rationalToDecimal (num*5) denq (dexp-1)
        | otherwise = Nothing
    ilog10 :: Integer -> Int -> Int
    ilog10 x dexp
        | x < 10 = dexp
        | otherwise = ilog10 (x `div` 10) (dexp + 1)
    dtos :: Integer -> Int -> Int -> String -> String
    dtos i digits decpt rest
        | i == 0 && digits <= 0 && decpt < 0 = rest
        | decpt == 0 = dtos i digits (-1) ('.':rest)
        | i >= 0 = do
            let (i', d) = i `divMod` 10
            let ch = toEnum $ fromEnum d + fromEnum '0'
            dtos i' (digits-1) (decpt-1) (ch:rest)
        | otherwise = undefined
    itos :: Int -> Int -> String -> String
    itos i digits rest
        | i == 0 && digits <= 0 = rest
        | i >= 0 = do
            let (i', d) = i `divMod` 10
            let ch = toEnum $ fromEnum d + fromEnum '0'
            itos i' (digits-1) (ch:rest)
        | otherwise = undefined


instance Show (LuaValue q s) where
    show LNil = "nil"
    show (LBool False) = "false"
    show (LBool True) = "true"
    show (LInteger i) = show i
    show (LRational r) = showRational r
    show (LDouble d) = show d
    show (LString s) = "\"" ++ qstr (BSt.unpack s) ++ "\""
        where
        qstr "" = ""
        qstr (c:cs)
            | c == '\n' =
                '\\':'n'
                :qstr cs
            | c == '\r' =
                '\\':'r'
                :qstr cs
            | c < '\10' && not (dstart cs) =
                '\\'
                :d cn
                :qstr cs
            | c < '\32' && not (dstart cs) =
                '\\'
                :d (cn `div` 10)
                :d (cn `mod` 10)
                :qstr cs
            | c < '\32' =
                '\\':'0'
                :d (cn `div` 10)
                :d (cn `mod` 10)
                :qstr cs
            | c > '\126' =
                '\\'
                :d (cn `div` 100)
                :d (cn `div` 10 `mod` 10)
                :d (cn `mod` 10)
                :qstr cs
            | c == '\\' = '\\':'\\':qstr cs
            | c == '"' = '\\':'"':qstr cs
            | otherwise = c:qstr cs
            where
            cn = fromEnum c
            d n = toEnum (n + fromEnum '0')
        dstart "" = False
        dstart (c:_) = '0' <= c && c <= '9'
    show (LFunction (LuaId i) _) = "<function: #" ++ show i ++ ">"
    show (LThread (LuaId i) _) = "<thread: #" ++ show i ++ ">"
    show (LTable (LuaId i) _) = "<table: #" ++ show i ++ ">"
    show (LUserdata (LuaId i) x)
        = lmtShow x $
            "<" ++ BSt.unpack (fromMaybe "userdata" (lmtTypename x))
            ++ ": #" ++ show i ++ ">"


lvalTypename :: LuaValue q s -> BSt.ByteString
lvalTypename LNil = "nil"
lvalTypename (LBool False) = "boolean"
lvalTypename (LBool True) = "boolean"
lvalTypename (LInteger _) = "number"
lvalTypename (LRational _) = "number"
lvalTypename (LDouble _) = "number"
lvalTypename (LString _) = "string"
lvalTypename (LFunction _ _) = "function"
lvalTypename (LThread _ _) = "thread"
lvalTypename (LTable _ _) = "table"
lvalTypename (LUserdata _ x) = fromMaybe "userdata" (lmtTypename x)


data LuaMetatypeWrapper q s where
    LuaMetatypeWrapper
        :: (   forall u . (forall t . LuaMetatype t => t q s -> u)
            -> LuaValue q s -> u)
        -> LuaMetatypeWrapper q s


data LuaRegistry q s where
    LuaRegistry :: Typeable t => t q s -> LuaRegistry q s


data LuaEnvironment q s = LuaEnvironment {
    lenvCounter
        :: LuaRef q s Int,
    lenvRegistry
        :: LuaRef q s (M.Map SomeTypeRep (LuaRegistry q s)),
    lenvNilMetatable
        :: LuaRef q s (LuaMetatypeWrapper q s),
    lenvBoolMetatable
        :: LuaRef q s (LuaMetatypeWrapper q s),
    lenvNumberMetatable
        :: LuaRef q s (LuaMetatypeWrapper q s),
    lenvStringMetatable
        :: LuaRef q s (LuaMetatypeWrapper q s),
    lenvFunctionMetatable
        :: LuaRef q s (LuaMetatypeWrapper q s),
    lenvThreadMetatable
        :: LuaRef q s (LuaMetatypeWrapper q s),
    lenvWarnHandler
        :: LuaRef q s (LuaValue q s -> LuaState q s ()),
    lenvDebugHook
        :: LuaRef q s (LuaValue q s, Bool, Bool, Bool),
    lenvStackOuterDepth
        :: LuaRef q s Int,
    lenvStackLimit
        :: LuaRef q s Int}


type LuaErrorHandler q s = LuaValue q s -> LuaState q s (LuaValue q s)


type LuaVariableList q s = [(
    Maybe (SourceRange, BSt.ByteString),
    LuaRef q s (LuaValue q s))]


data LuaStackFrame q s = LuaStackFrame {
    lsfDefinition :: !(Maybe (SourceRange, BSt.ByteString)),
    lsfCurrentLocation :: !(LuaRef q s (Maybe SourceRange)),
    lsfUpvalues :: !(LuaVariableList q s),
    lsfLocals :: !(LuaVariableList q s)}


lsfModifyLocals
    :: (LuaVariableList q s -> LuaVariableList q s)
    -> LuaStackFrame q s
    -> LuaStackFrame q s
lsfModifyLocals f (LuaStackFrame d c u l)
    = LuaStackFrame d c u (f l)


data LuaContext q s = LuaContext {
    lctxEnvironment :: !(LuaEnvironment q s),
    lctxErrHandler :: LuaErrorHandler q s,
    lctxCurrentThread :: LuaValue q s,
    lctxStack :: ![LuaStackFrame q s],
    lctxStackInnerDepth :: !Int,
    lctxOnClose :: !(LuaCloseChain q s),
    lctxInsideHook :: !Bool}


lctxModifyStack
    :: ([LuaStackFrame q s] -> [LuaStackFrame q s])
    -> LuaContext q s
    -> LuaContext q s
lctxModifyStack f lctx = lctx {
    lctxStack = f $ lctxStack lctx}


type LuaRef q s = STRef s


data LuaLifted s t where
    LuaLiftedST :: ST s t -> LuaLifted s t
    LuaLiftedIO :: IO t -> LuaLifted s (Maybe t)


newtype LuaState q s t = LuaState (LuaContext q s -> LuaCoroutine q s t)


instance Functor (LuaState q s) where
    fmap f (LuaState st) = LuaState $ \ !ctx -> do
        fmap f $ st ctx


instance Applicative (LuaState q s) where
    pure x = LuaState $ \ _ -> do
        pure $ x
    LuaState sta <*> LuaState stb = LuaState $ \ !ctx -> do
        sta ctx <*> stb ctx


instance Monad (LuaState q s) where
    return x = LuaState $ \ _ -> do
        pure $ x
    LuaState st1 >>= f = LuaState $ \ !ctx -> do
        mid <- st1 ctx
        let LuaState st2 = f mid
        st2 ctx


lxRunT
    :: (forall u . ST s u -> (u -> r) -> r)
    -> (forall u . IO u -> (Maybe u -> r) -> r)
    -> (forall q . LuaValue q s -> r)
    -> (t -> r)
    -> (forall q . LuaState q s t)
    -> r
lxRunT onST onIO onError onPure (LuaState lstate) = do
    onST
        (LuaEnvironment
            <$> newSTRef 2
            <*> newSTRef M.empty
            <*> newSTRef lxDefaultMetatable
            <*> newSTRef lxDefaultMetatable
            <*> newSTRef lxDefaultMetatable
            <*> newSTRef lxDefaultMetatable
            <*> newSTRef lxDefaultMetatable
            <*> newSTRef lxDefaultMetatable
            <*> (newSTRef =<< makeDefaultWarnHandler)
            <*> newSTRef (LNil, False, False, False)
            <*> newSTRef 0
            <*> newSTRef 4000) $
        \env -> do
            onST (newSTRef $! LTRunning False) $ \pthread -> do
                let !ctx = LuaContext {
                    lctxEnvironment = env,
                    lctxErrHandler = pure,
                    lctxCurrentThread = LThread (LuaId 1) (LuaThread pthread),
                    lctxStack = [],
                    lctxStackInnerDepth = 0,
                    lctxOnClose = mempty,
                    lctxInsideHook = False}
                Y.execute (lstate ctx)
                    (\box cont -> do
                        case box of
                            LuaLiftedST st -> onST st cont
                            LuaLiftedIO io -> onIO io cont)
                    onError
                    onPure


makeDefaultWarnHandler :: ST s (LuaValue q s -> LuaState q s ())
makeDefaultWarnHandler = do
    isActiveRef <- newSTRef True
    return (\a ->
        case a of
            LString msg -> processWarnMsg isActiveRef msg
            _ -> return ())
    where
    processWarnMsg isActiveRef msg
        | msg == "@off" = lxWrite isActiveRef $ False
        | msg == "@on" = lxWrite isActiveRef $ True
        | "@" `BSt.isPrefixOf` msg = return ()
        | otherwise = do
            isActive <- lxRead isActiveRef
            if isActive
                then do
                    let msgStr = BSt.unpack msg
                    _ <- lxTryLiftIO $ putStrLn $
                        "Lua warning: " ++ msgStr
                    return ()
                else return ()


lxContext :: LuaState q s (LuaContext q s)
lxContext = LuaState $ \ !ctx -> pure ctx


lxLocalContext
    :: (LuaContext q s -> LuaContext q s)
    -> LuaState q s t
    -> LuaState q s t
lxLocalContext f (LuaState lstate) = LuaState $ \ !ctx -> do
    lstate $! f ctx


lxYield :: [LuaValue q s] -> LuaState q s [LuaValue q s]
lxYield bvals = do
    ~LuaContext {
            lctxCurrentThread = LThread _ currentthread,
            lctxOnClose = onclose}
        <- lxContext
    isyieldable <- lxIsYieldable currentthread
    if isyieldable
        then lxLiftY $ Y.yield (onclose, bvals)
        else lxError $ errWrongYield


lxErrorAt :: Int -> LuaValue q s -> LuaState q s a
lxErrorAt level err0 = do
    LuaContext {
            lctxErrHandler = errh,
            lctxStack = stack}
        <- lxContext
    err1 <- case err0 of
        LString msg | level > 0 -> do
            case drop level $ stack of
                [] -> return $ err0
                LuaStackFrame {lsfCurrentLocation = ploc}:_ -> do
                    mloc <- lxRead ploc
                    case mloc of
                        Just pr@(SourceRange (_, Just _)) -> do
                            return $ LString $ BSt.pack $
                                show pr ++ ": " ++ BSt.unpack msg
                        _ -> return $ err0
        _ -> return $ err0
    err2 <- lxWithErrHandler return $ errh err1
    lxLiftY $ Y.raise err2


lxError :: LuaValue q s -> LuaState q s a
lxError = lxErrorAt 1


lxTry :: LuaState q s t -> LuaState q s (Either (LuaValue q s) t)
lxTry (LuaState act) = do
    LuaState $
        \ctx -> do
            Y.catch
                (Right <$> act ctx)
                (return . Left)


lxStackLevel :: LuaState q s t -> LuaState q s t
lxStackLevel act = do
    LuaContext {
            lctxEnvironment = LuaEnvironment {
                lenvStackOuterDepth = pouterdepth,
                lenvStackLimit = plimit},
            lctxStackInnerDepth = innerdepth}
        <- lxContext
    outerdepth <- lxRead $ pouterdepth
    limit <- lxRead $ plimit
    if innerdepth + outerdepth < limit
        then do
            lxLocalContext (\ctx -> ctx {lctxStackInnerDepth = innerdepth+1}) $
                act
        else lxError $ errStack


lxSetStackLimit :: Int -> LuaState q s (Maybe Int)
lxSetStackLimit newlimit = do
    LuaContext {
            lctxEnvironment = LuaEnvironment {
                lenvStackOuterDepth = pouterdepth,
                lenvStackLimit = plimit},
            lctxStackInnerDepth = innerdepth}
        <- lxContext
    outerdepth <- lxRead $ pouterdepth
    oldlimit <- lxRead $ plimit
    if outerdepth == 0 && innerdepth < newlimit && newlimit <= 40000
        then do
            lxWrite plimit $ newlimit
            return $ Just oldlimit
        else return $ Nothing


lxWithErrHandler
    :: LuaErrorHandler q s
    -> LuaState q s t
    -> LuaState q s t
lxWithErrHandler errh act = do
    lxLocalContext (\ctx -> ctx {lctxErrHandler = errh}) $ act


lxWarn :: LuaValue q s -> LuaState q s ()
lxWarn e = do
    env <- lctxEnvironment <$> lxContext
    warnh <- lxRead $ lenvWarnHandler env
    _ <- lxTry (warnh e)
    return ()


lxFinally
    :: LuaState q s t
    -> (LuaValue q s -> LuaState q s ())
    -> LuaState q s t
lxFinally act fin = do
    ctx <- lxContext
    let onclosemine merr = do
        case merr of
            Nothing -> do
                fr <- lxTry $ fin LNil
                case fr of
                    Left err -> return $ Just err
                    Right _ -> return $ merr
            Just err -> do
                fr <- lxTry $ fin err
                case fr of
                    Left err2 -> lxWarn err2
                    Right _ -> return ()
                return $ merr
    let chainelem = LuaCloseChain $ \merr -> do
        case onclosemine merr of
            LuaState lstate -> lstate ctx
    ar <- lxLocalContext
        (\myctx -> myctx {
            lctxOnClose = chainelem <> lctxOnClose myctx}) $
        lxTry act
    let LThread _ (LuaThread pcurrent) = lctxCurrentThread ctx
    ~(LTRunning isyieldable) <- lxRead pcurrent
    lxWrite pcurrent $ (LTRunning False)
    result <- case ar of
        Left aerr -> do
            fr <- lxTry $ fin aerr
            case fr of
                Left ferr -> lxWarn ferr
                Right _ -> return ()
            lxLiftY $ Y.raise aerr
        Right asuc -> do
            fr <- lxTry $ fin LNil
            case fr of
                Left ferr -> do
                    fr2 <- lxTry $ fin ferr
                    case fr2 of
                        Left ferr2 -> lxWarn ferr2
                        Right _ -> return ()
                    lxLiftY $ Y.raise ferr
                Right _ -> return $ asuc
    lxWrite pcurrent $ (LTRunning isyieldable)
    return $ result


lxLiftY :: LuaCoroutine q s a -> LuaState q s a
lxLiftY cor = LuaState $ \_ -> cor


lxLiftST :: ST s a -> LuaState q s a
lxLiftST st = lxLiftY $ Y.lift $ LuaLiftedST st


lxTryLiftIO :: IO a -> LuaState q s (Maybe a)
lxTryLiftIO act = lxLiftY $ Y.lift $ LuaLiftedIO act


lxAlloc :: a -> LuaState q s (LuaRef q s a)
lxAlloc x = lxLiftST $ newSTRef x


lxRead :: LuaRef q s a -> LuaState q s a
lxRead ref = lxLiftST $ readSTRef ref


lxWrite :: LuaRef q s a -> a -> LuaState q s ()
lxWrite ref !x = lxLiftST $ writeSTRef ref x


lxNewThread
    :: LuaFunction q s
    -> LuaState q s (LuaValue q s)
lxNewThread func = do
    env <- lctxEnvironment <$> lxContext
    threadid <- lxNewId
    pthread <- lxAlloc $ LTDead
    let value = LThread threadid (LuaThread pthread)
    suspended <- lxLiftY $ Y.suspend $ \args -> do
        let LuaState lstate = func args
        let ctx = LuaContext {
            lctxEnvironment = env,
            lctxErrHandler = pure,
            lctxCurrentThread = value,
            lctxStack = [],
            lctxStackInnerDepth = 1,
            lctxOnClose = mempty,
            lctxInsideHook = False}
        result <- lstate $! ctx
        return $ (mempty, result)
    lxWrite pthread $ LTSuspended mempty suspended
    return $ value


lxResume
    :: LuaThread q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
lxResume (LuaThread pthread) ivals = do
    lxStackLevel $ do
        state <- lxRead pthread
        case state of
            LTSuspended _ suspended -> do
                resumeThread suspended
            _ -> return $ Left errNonSuspended
    where
    resumeThread suspended = do
        ~LuaContext {
                lctxEnvironment = LuaEnvironment {
                    lenvStackOuterDepth = pouterdepth},
                lctxCurrentThread = LThread _ (LuaThread pcurrent),
                lctxStackInnerDepth = innerdepth}
            <- lxContext
        outerdepth <- lxRead pouterdepth
        ~(LTRunning isyieldable) <- lxRead pcurrent
        lxWrite pouterdepth $ outerdepth + innerdepth
        lxWrite pcurrent $ LTNormal isyieldable
        lxWrite pthread $ LTRunning True
        (result, rtstate) <- lxLiftY $ Y.resume
            suspended
            (\(onclose, ovals) suspended2 -> do
                return $ (Right ovals, LTSuspended onclose suspended2))
            (\err -> do
                return $ (Left err, LTDead))
            (\(_, ovals) -> do
                return $ (Right ovals, LTDead))
            ivals
        lxWrite pthread $ rtstate
        lxWrite pcurrent $ LTRunning isyieldable
        lxWrite pouterdepth $ outerdepth
        return $ result


lxThreadClose
    :: LuaThread q s
    -> LuaState q s (Maybe (LuaValue q s))
lxThreadClose (LuaThread pthread) = do
    state <- lxRead pthread
    case state of
        LTRunning _ -> lxError $ errWrongThreadClose
        LTSuspended (LuaCloseChain onclose) _ -> do
            ~LuaContext {
                    lctxEnvironment = LuaEnvironment {
                        lenvStackOuterDepth = pouterdepth},
                    lctxCurrentThread = LThread _ (LuaThread pcurrent),
                    lctxStackInnerDepth = innerdepth}
                <- lxContext
            outerdepth <- lxRead pouterdepth
            ~(LTRunning isyieldable) <- lxRead pcurrent
            lxWrite pouterdepth $ outerdepth + innerdepth
            lxWrite pcurrent $ LTNormal isyieldable
            lxWrite pthread $ LTRunning False
            result <- lxLiftY $ onclose Nothing
            lxWrite pthread $ LTDead
            lxWrite pcurrent $ LTRunning isyieldable
            lxWrite pouterdepth $ outerdepth
            return $ result
        LTNormal _ -> lxError $ errWrongThreadClose
        LTDead -> return Nothing


lxThreadState
    :: LuaThread q s
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
lxThreadState (LuaThread pthread) onRunning onSuspended onNormal onDead = do
    state <- lxRead pthread
    case state of
        LTRunning _ -> onRunning
        LTSuspended _ _ -> onSuspended
        LTNormal _ -> onNormal
        LTDead -> onDead


lxNewId :: LuaState q s LuaId
lxNewId = do
    ref <- lenvCounter . lctxEnvironment <$> lxContext
    n <- lxRead ref
    lxWrite ref $ n + 1
    return $ LuaId $ n + 1


lxKey :: LuaValue q s -> T.Key
lxKey a = do
    case a of
        LNil -> T.KNil
        LBool b -> T.KBool b
        LInteger i -> asInteger i
        LRational r -> asRational r
        LDouble d
            | isNaN d -> T.KNaN
            | isInfinite d && d > 0 -> T.KPosInf
            | isInfinite d -> T.KNegInf
            | otherwise -> asRational $ toRational d
        LString str -> T.KString $ str
        LFunction (LuaId i) _ -> T.KId $ i
        LThread (LuaId i) _ -> T.KId $ i
        LTable (LuaId i) _ -> T.KId $ i
        LUserdata (LuaId i) _ -> T.KId $ i
    where
    asInteger !i
        | mini <= i && i <= maxi = T.KInt (fromInteger i)
        | otherwise = T.KNumber $ toRational i
    asRational !r
        | denominator r == 1 = asInteger (numerator r)
        | otherwise = T.KNumber $ r
    mini = toInteger (minBound :: Int)
    maxi = toInteger (maxBound :: Int)


lxAllocTable
    :: [(LuaValue q s, LuaValue q s)]
    -> LuaState q s (LuaTable q s)
lxAllocTable elems = do
    body <- do
        body' <- lxLiftST $ T.new
        flip fix elems $ \loop elems1 -> do
            case elems1 of
                (LNil, _):_ -> lxError $ errNilIndex
                (LDouble d, _):_ | isNaN d -> lxError $ errNanIndex
                (_, LNil):rest -> loop rest
                (!kval, !vval):rest -> do
                    lxLiftST $ T.set body' (lxKey kval) (kval, vval)
                    loop rest
                [] -> return ()
        return $ body'
    metaw <- lxAlloc $ lxDefaultMetatable
    return $! LuaTable body metaw


lxTableGet
    :: LuaTable q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
lxTableGet (LuaTable body _) !key = do
    snd <$> (lxLiftST $ T.get body (lxKey key))


lxTableSet
    :: LuaTable q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
lxTableSet (LuaTable body _) !key !value = do
    lxLiftST $ T.set body (lxKey key) (key, value)


lxTableLength
    :: LuaTable q s
    -> LuaState q s Integer
lxTableLength (LuaTable body _) = do
    toInteger <$> (lxLiftST $ T.length body)


data LuaIterator q s = LuaIterator
    (T.Table s (LuaValue q s, LuaValue q s))
    [T.Key]


lxTableIter
    :: LuaTable q s
    -> LuaState q s (LuaIterator q s)
lxTableIter (LuaTable body _) = do
    ks <- lxLiftST $ T.keys body
    return $! LuaIterator body ks


lxTableNext
    :: LuaIterator q s
    -> LuaState q s a
    -> (LuaIterator q s -> LuaValue q s -> LuaValue q s -> LuaState q s a)
    -> LuaState q s a
lxTableNext (LuaIterator body ks1) onEnd onIter = do
    go ks1
    where
    go ks = do
        case ks of
            [] -> onEnd
            (!k):rest -> do
                (!key, !value) <- lxLiftST $ T.get body k
                case value of
                    LNil -> go rest
                    _ -> do
                        let !newiter = LuaIterator body rest
                        onIter newiter key value


lxExtend :: Int -> [LuaValue q s] -> [LuaValue q s]
lxExtend 0 xs = xs
lxExtend n [] = LNil:lxExtend (n-1) []
lxExtend n (x:xs) = x:lxExtend (n-1) xs


lxCar :: [LuaValue q s] -> LuaValue q s
lxCar [] = LNil
lxCar (a:_) = a


lxSetNilMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetNilMetatable mt = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvNilMetatable env) mt


lxSetBoolMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetBoolMetatable mt = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvBoolMetatable env) mt


lxSetNumberMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetNumberMetatable mt = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvNumberMetatable env) mt


lxSetStringMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetStringMetatable mt = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvStringMetatable env) mt


lxSetFunctionMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetFunctionMetatable mt = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvFunctionMetatable env) mt


lxSetThreadMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetThreadMetatable mt = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvThreadMetatable env) mt


lxSetWarnHandler
    :: (LuaValue q s -> LuaState q s ())
    -> LuaState q s ()
lxSetWarnHandler wh = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvWarnHandler env) wh


lxGetDebugHook
    :: LuaState q s (LuaValue q s, Bool, Bool, Bool)
lxGetDebugHook = do
    env <- lctxEnvironment <$> lxContext
    lxRead (lenvDebugHook env)


lxSetDebugHook
    :: (LuaValue q s, Bool, Bool, Bool)
    -> LuaState q s ()
lxSetDebugHook dh = do
    env <- lctxEnvironment <$> lxContext
    lxWrite (lenvDebugHook env) dh


lxDefaultMetatable
    :: LuaMetatypeWrapper q s
lxDefaultMetatable = LuaMetatypeWrapper (. LuaUnannotated)


lxProduceMetatable
    :: LuaValue q s
    -> LuaMetatypeWrapper q s
lxProduceMetatable LNil = do
    lxDefaultMetatable
lxProduceMetatable (LTable lid table) = do
    LuaMetatypeWrapper (. LuaAnnotated lid table)
lxProduceMetatable _ = undefined


lxMetatable
    :: LuaValue q s
    -> (forall t . (LuaMetatype t)
        => t q s
        -> LuaState q s a)
    -> LuaState q s a
lxMetatable x f = do
    case x of
        LNil -> global lenvNilMetatable
        LBool _ -> global lenvBoolMetatable
        LInteger _ -> global lenvNumberMetatable
        LRational _ -> global lenvNumberMetatable
        LDouble _ -> global lenvNumberMetatable
        LString _ -> global lenvStringMetatable
        LFunction _ _ -> global lenvFunctionMetatable
        LThread _ _ -> global lenvThreadMetatable
        LTable _ (LuaTable _ pmeta) -> do
            LuaMetatypeWrapper w <- lxRead pmeta
            w f x
        LUserdata _ ud -> f ud
    where
    global accf = do
        state <- lctxEnvironment <$> lxContext
        LuaMetatypeWrapper w <- lxRead $ accf state
        w f x


lxPerformMetaopUnary
    :: (forall t . LuaMetatype t => LuaMetaopUnary r q s t)
    -> LuaValue q s
    -> LuaState q s r
    -> LuaState q s r
lxPerformMetaopUnary metaf a def = do
    lxMetatable a (\x -> metaf x $
        def)


lxPerformMetaopBinary
    :: (forall t . LuaMetatype t => LuaMetaopBinary r q s t)
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s r
    -> LuaState q s r
lxPerformMetaopBinary metaf a b def = do
    lxMetatable a (\x -> metaf x b False $
        lxMetatable b (\y -> metaf y a True $
            def))


lxPerformUnary
    :: (LuaValue q s -> LuaState q s r -> LuaState q s r)
    -> (forall t . LuaMetatype t => LuaMetaopUnary r q s t)
    -> LuaValue q s
    -> LuaState q s r
    -> LuaState q s r
lxPerformUnary raw metaf a def = do
    raw a $
        lxPerformMetaopUnary metaf a $
            def


lxPerformBinary
    :: (LuaValue q s -> LuaValue q s -> LuaState q s r -> LuaState q s r)
    -> (forall t . LuaMetatype t => LuaMetaopBinary r q s t)
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s r
    -> LuaState q s r
lxPerformBinary raw metaf a b def = do
    raw a b $
        lxPerformMetaopBinary metaf a b $
            def


lxToNumber
    :: LuaValue q s
    -> a
    -> (Integer -> a)
    -> (Rational -> a)
    -> (Double -> a)
    -> a
lxToNumber a onOther onInteger onRational onDouble = do
    case a of
        LInteger i -> onInteger i
        LRational q -> onRational q
        LDouble d -> onDouble d
        _ -> onOther


lxToNumber2
    :: LuaValue q s
    -> LuaValue q s
    -> a
    -> a
    -> (Integer -> Integer -> a)
    -> (Rational -> Rational -> a)
    -> (Double -> Double -> a)
    -> a
lxToNumber2 a b onOtherA onOtherB onInteger2 onRational2 onDouble2 = do
    lxToNumber a
        onOtherA
        (\ia -> lxToNumber b
            onOtherB
            (\ib -> onInteger2 ia ib)
            (\qb -> onRational2 (fromInteger ia) qb)
            (\db -> onDouble2 (fromInteger ia) db))
        (\qa -> lxToNumber b
            onOtherB
            (\ib -> onRational2 qa (fromInteger ib))
            (\qb -> onRational2 qa qb)
            (\db -> onDouble2 (fromRational qa) db))
        (\da -> lxToNumber b
            onOtherB
            (\ib -> onDouble2 da (fromInteger ib))
            (\qb -> onDouble2 da (fromRational qb))
            (\db -> onDouble2 da db))


lxRawArithUnm
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithUnm a def = do
    lxToNumber a
        def
        (return . LInteger . negate)
        (return . LRational . negate)
        (return . LDouble . negate)


lxRawArithBNot
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithBNot a def = do
    case a of
        LInteger x  -> (LInteger . complement) <$> cvInt x
        LRational x -> (LInteger . complement) <$> cvRat x
        LDouble x   -> (LInteger . complement) <$> cvDbl x
        _ -> def
    where
    cvInt x = return x
    cvRat x =
        let d = toRational x in
            if denominator d /= 1
                then lxError $ errWrongBit
                else return $ numerator d
    cvDbl x =
        if isNaN x || isInfinite x
            then lxError $ errWrongBit
            else cvRat (toRational x)


lxRawArithNum
    :: (forall t . (Num t) => t -> t -> t)
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithNum op a b def = do
    lxToNumber2 a b
        def
        def
        (\x y -> return $ LInteger $ op x y)
        (\x y -> return $ LRational $ op x y)
        (\x y -> return $ LDouble $ op x y)


lxRawArithDiv
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithDiv a b def = do
    lxToNumber2 a b
        def
        def
        doInteger
        doRational
        doDouble
    where
    doInteger x y =
        if y == 0
            then doDouble (fromInteger x) (fromInteger y)
            else return $ LRational $ (fromInteger x) / (fromInteger y)
    doRational x y =
        if y == 0
            then doDouble (fromRational x) (fromRational y)
            else return $ LRational $ x / y
    doDouble x y =
        return $ LDouble $ x / y


lxRawArithIDiv
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithIDiv a b def = do
    lxToNumber2 a b
        def
        def
        doInteger
        doRational
        doDouble
    where
    doInteger x y =
        if y == 0
            then lxError $ errDivideZero
            else return $ LInteger $ div x y
    doRational x y =
        if y == 0
            then doDouble (fromRational x) (fromRational y)
            else return $ LRational $ fromInteger (floor (x / y))
    doDouble x y =
        return $ LDouble $ let r = x / y in
            if isNaN r || isInfinite r || isNegativeZero r
                then r
                else fromInteger (floor r)


lxRawArithMod
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithMod a b def = do
    lxToNumber2 a b
        def
        def
        doInteger
        doRational
        doDouble
    where
    doInteger x y
        | y == 0 = lxError $ errDivideZero
        | otherwise = return $ LInteger $ mod x y
    doRational x y = do
        case rationalmod x y of
            Nothing -> return $ LDouble $ 0/0
            Just r -> return $ LRational $ r
    doDouble x y
        | isInfinite x || isNaN x || isNaN y = return $ LDouble $ 0/0
        | isInfinite y = if x >= 0 && y > 0 || x <= 0 && y < 0
            then return $ LDouble $ x
            else return $ LDouble $ y
        | otherwise = do
            case rationalmod (toRational x) (toRational y) of
                Nothing -> return $ LDouble $ 0/0
                Just 0 -> if x < 0
                    then return $ LDouble $ -0
                    else return $ LDouble $ 0
                Just r -> return $ LDouble $ fromRational r
    rationalmod x y
        | y == 0 = Nothing
        | otherwise = Just $ x - y * toRational (floor (x / y) :: Integer)


lxRawArithPow
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithPow a b def = do
    case (a, b) of
        (LInteger  x, LInteger  y) -> doDouble (fromInteger x) (fromInteger y)
        (LInteger  x, LRational y) -> doDouble (fromInteger x) (fromRational y)
        (LInteger  x, LDouble   y) -> doDouble (fromInteger x) y
        (LRational x, LInteger  y) -> doDouble (fromRational x) (fromInteger y)
        (LRational x, LRational y) -> doDouble (fromRational x) (fromRational y)
        (LRational x, LDouble   y) -> doDouble (fromRational x) y
        (LDouble   x, LInteger  y) -> doDouble x (fromInteger y)
        (LDouble   x, LRational y) -> doDouble x (fromRational y)
        (LDouble   x, LDouble   y) -> doDouble x y
        _ -> def
    where
    doDouble x y =
        return $ LDouble $ x ** y


lxRawArithBitwise
    :: (Integer -> Integer -> Integer)
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithBitwise op a b def = do
    case (a, b) of
        (LInteger  x, LInteger  y) ->
            LInteger <$> (op <$> (cvInt x) <*> (cvInt y))
        (LInteger  x, LRational y) ->
            LInteger <$> (op <$> (cvInt x) <*> (cvRat y))
        (LInteger  x, LDouble   y) ->
            LInteger <$> (op <$> (cvInt x) <*> (cvDbl y))
        (LRational x, LInteger  y) ->
            LInteger <$> (op <$> (cvRat x) <*> (cvInt y))
        (LRational x, LRational y) ->
            LInteger <$> (op <$> (cvRat x) <*> (cvRat y))
        (LRational x, LDouble   y) ->
            LInteger <$> (op <$> (cvRat x) <*> (cvDbl y))
        (LDouble   x, LInteger  y) ->
            LInteger <$> (op <$> (cvDbl x) <*> (cvInt y))
        (LDouble   x, LRational y) ->
            LInteger <$> (op <$> (cvDbl x) <*> (cvRat y))
        (LDouble   x, LDouble   y) ->
            LInteger <$> (op <$> (cvDbl x) <*> (cvDbl y))
        _ -> def
    where
    cvInt x = return x
    cvRat x =
        let d = toRational x in
            if denominator d /= 1
                then lxError $ errWrongBit
                else return $ numerator d
    cvDbl x =
        if isNaN x || isInfinite x
            then lxError $ errWrongBit
            else cvRat (toRational x)


lxRawCompare
    :: (forall t . (Ord t) => t -> t -> Bool)
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s Bool
    -> LuaState q s Bool
lxRawCompare op a b def = do
    case (a, b) of
        (LInteger  x, LInteger  y) -> return $ op x y
        (LInteger  x, LRational y) -> return $ op (fromInteger x) y
        (LInteger  x, LDouble   y) -> return $ doRatDbl (fromInteger x) y
        (LRational x, LInteger  y) -> return $ op x (fromInteger y)
        (LRational x, LRational y) -> return $ op x y
        (LRational x, LDouble   y) -> return $ doRatDbl x y
        (LDouble   x, LInteger  y) -> return $ doDblRat x (fromInteger y)
        (LDouble   x, LRational y) -> return $ doDblRat x y
        (LDouble   x, LDouble   y) -> return $ op x y
        (LString x, LString y) -> return $ op x y
        _ -> def
    where
    doRatDbl x y =
        if isNaN y
            then False
            else if isInfinite y
                then op 0 y
                else op x (toRational y)
    doDblRat x y =
        if isNaN x
            then False
            else if isInfinite x
                then op x 0
                else op (toRational x) y


lxRawCompareEq
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s Bool
    -> LuaState q s Bool
lxRawCompareEq a b def = do
    let ka = lxKey a
    let kb = lxKey b
    if ka == T.KNaN || kb == T.KNaN
        then return $ False
        else if ka == kb
            then return $ True
            else case (a, b) of
                (LTable _ _, _) -> def
                (LUserdata _ _, _) -> def
                (_, LTable _ _) -> def
                (_, LUserdata _ _) -> def
                _ -> return $ False


lxRawLen
    :: LuaValue q s
    -> LuaState q s Integer
    -> LuaState q s Integer
lxRawLen a def = do
    case a of
        LString x -> return $ toInteger (BSt.length x)
        LTable _ table -> lxTableLength table
        _ -> def


lxCall
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s [LuaValue q s]
lxCall a args = do
    case a of
        LFunction _ f -> f args
        _ -> do
            lxMetatable a (\x -> lmtCall x args $
                lxError $ errWrongCall a)


lxAsString
    :: LuaValue q s
    -> LuaState q s BSt.ByteString
lxAsString a = do
    case a of
        LNil -> def
        LBool _ -> def
        LInteger _ -> def
        LRational _ -> def
        LDouble _ -> def
        LString s -> return $ s
        _ -> do
            er <- lxTry $ lxMetatable a (\x -> lmtAsString x $ def)
            case er of
                Left err -> do
                    lxWarn err
                    def
                Right r -> return $ r
    where
    def = return $ BSt.pack $ show a


lxGet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
lxGet a b = do
    case a of
        LTable _ table -> do
            case b of
                LNil -> metaGet $ return LNil
                LDouble d | isNaN d -> metaGet $ return LNil
                _ -> do
                    result <- lxTableGet table b
                    case result of
                        LNil -> metaGet $ return LNil
                        _ -> return $ result
        _ -> metaGet $ lxError $ errWrongTable a
    where
    metaGet def = do
        lxMetatable a (\x -> lmtIndex x b $ def)


lxSet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
lxSet a b c = do
    case a of
        LTable _ table -> do
            case b of
                LNil -> metaSet $ lxError $ errNilIndex
                LDouble d | isNaN d -> metaSet $ lxError $ errNanIndex
                _ -> do
                    result <- lxTableGet table b
                    case result of
                        LNil -> lxMetatable a (\x ->
                            lmtNewIndex x b c $
                                lxTableSet table b c)
                        _ -> lxTableSet table b c
        _ -> metaSet $ lxError $ errWrongTable a
    where
    metaSet def = do
        lxMetatable a (\x -> do
            lmtNewIndex x b c $ def)


lxAskStack
    :: LuaState q s [LuaStackFrame q s]
lxAskStack = do
    lctxStack <$> lxContext


lxLocalStack
    :: ([LuaStackFrame q s] -> [LuaStackFrame q s])
    -> LuaState q s t
    -> LuaState q s t
lxLocalStack func act = do
    lxLocalContext (lctxModifyStack func) act


lxRunHook
    :: LuaState q s ()
    -> LuaState q s ()
lxRunHook (LuaState act) = LuaState $ \ !ctx -> do
    if lctxInsideHook ctx
        then return ()
        else act $ ctx {lctxInsideHook = True}


lxCurrentThread
    :: LuaState q s (LuaValue q s, Bool)
lxCurrentThread = do
    value <- lctxCurrentThread <$> lxContext
    case value of
        LThread (LuaId 1) _ -> return $ (value, True)
        _ -> return $ (value, False)


lxIsYieldable
    :: LuaThread q s
    -> LuaState q s Bool
lxIsYieldable (LuaThread pthread) = do
    state <- lxRead pthread
    case state of
        LTRunning b -> return $ b
        LTNormal b -> return $ b
        _ -> return $ True


lxRegistrySet
    :: Typeable t
    => Maybe (t q s)
    -> LuaState q s ()
lxRegistrySet mx = do
    preg <- lenvRegistry . lctxEnvironment <$> lxContext
    reg1 <- lxRead preg
    let reg2 = case mx of
            Nothing -> do
                M.delete trep reg1
            Just x -> do
                M.insert trep (LuaRegistry x) reg1
    lxWrite preg reg2
    where
    trep = someTypeRep $ myproxy mx
    myproxy :: Maybe (t q s) -> Proxy t
    myproxy _ = Proxy


lxRegistryGet
    :: Typeable t
    => LuaState q s (Maybe (t q s))
lxRegistryGet = do
    preg <- lenvRegistry . lctxEnvironment <$> lxContext
    reg <- lxRead preg
    return $ proxyCont $ \myproxy -> do
        case M.lookup (someTypeRep myproxy) reg of
            Just (LuaRegistry x) -> hcast x
            Nothing -> Nothing
    where
    proxyCont :: (Proxy t -> Maybe (t q s)) -> Maybe (t q s)
    proxyCont f = f Proxy
    hcastWith :: a q s -> (a :~~: b) -> b q s
    hcastWith x HRefl = x
    hcast :: (Typeable a, Typeable b) => a q s -> Maybe (b q s)
    hcast x = hcastWith x <$> eqTypeRep typeRep typeRep
