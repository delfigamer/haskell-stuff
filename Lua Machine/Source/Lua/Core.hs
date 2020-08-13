{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Lua.Core (
    LuaErrorHandler,
    LuaFunction,
    LuaIterator,
    LuaMetatype(..),
    LuaMetatypeWrapper(..),
    LuaPure(..),
    LuaRef,
    LuaStackFrame(..),
    LuaState,
    LuaTable(..),
    LuaValue(..),
    LuaVariableList,
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
    lsfModifyLocals,
    lvalTypename,
    lxAlloc,
    lxAllocTable,
    lxAsString,
    lxAskStack,
    lxCall,
    lxCar,
    lxDefaultMetatable,
    lxError,
    lxExtend,
    lxFinally,
    lxGet,
    lxGetDebugHook,
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
    lxResume,
    lxRunT,
    lxSet,
    lxSetBoolMetatable,
    lxSetDebugHook,
    lxSetFunctionMetatable,
    lxSetNilMetatable,
    lxSetNumberMetatable,
    lxSetStringMetatable,
    lxSetThreadMetatable,
    lxSetWarnHandler,
    lxTableGet,
    lxTableIter,
    lxTableLength,
    lxTableNext,
    lxTableSet,
    lxThreadState,
    lxTry,
    lxTryLiftIO,
    lxWithErrHandler,
    lxWrite,
    lxYield,
) where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.ST
import Data.Bits
import Data.Maybe
import Data.Ratio
import Data.STRef
import Type.Reflection
import qualified Data.ByteString.Char8 as BSt
import Lua.SourceRange
import qualified Lua.Coroutine as Y
import qualified Lua.Table as T


data LuaHost s t where
    LuaHostPure :: t -> LuaHost s t
    LuaHostLiftedST :: ST s a -> (a -> LuaHost s t) -> LuaHost s t
    LuaHostLiftedIO :: IO a -> (Maybe a -> LuaHost s t) -> LuaHost s t


lhostLiftST :: ST s t -> LuaHost s t
lhostLiftST act = LuaHostLiftedST act LuaHostPure


lhostLiftIO :: IO t -> LuaHost s (Maybe t)
lhostLiftIO act = LuaHostLiftedIO act LuaHostPure


instance Monad (LuaHost s) where
    return x = LuaHostPure x
    LuaHostPure x >>= f = f x
    LuaHostLiftedST act next >>= f = LuaHostLiftedST act (f <=< next)
    LuaHostLiftedIO act next >>= f = LuaHostLiftedIO act (f <=< next)


instance Functor (LuaHost s) where
    fmap f (LuaHostPure x) = LuaHostPure (f x)
    fmap f (LuaHostLiftedST act next) = LuaHostLiftedST act (fmap f . next)
    fmap f (LuaHostLiftedIO act next) = LuaHostLiftedIO act (fmap f . next)


instance Applicative (LuaHost s) where
    pure x = LuaHostPure x
    mx <*> my = mx >>= (\f -> fmap f my)


type LuaRef q s = STRef s


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
    lmtClose :: t q s -> LuaState q s ()
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
    lmtClose _self = return ()
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
        f -> lxCar <$> lxCall f [value]


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
            next -> lxGet next index
    lmtNewIndex !self !index !new def = do
        (value, metaop) <- iimtParse "__newindex" self
        case metaop of
            LNil -> def
            LFunction _ ff -> () <$ ff [value, index, new]
            next -> lxSet next index new
    lmtCall !self args def = do
        (value, metaop) <- iimtParse "__call" self
        case metaop of
            LNil -> def
            next -> lxCall next (value:args)
    lmtClose !self = do
        (value, metaop) <- iimtParse "__close" self
        case metaop of
            LNil -> return ()
            f -> () <$ lxCall f [value]
    lmtAsString !self def = do
        (value, metaop) <- iimtParse "__tostring" self
        case metaop of
            LNil -> def
            LFunction _ ff -> lxAsString . lxCar =<< ff [value]
            next -> lxAsString next


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


data LuaThreadState q s
    = LTRunning
    | LTSuspended
        !((Either (LuaValue q s) [LuaValue q s]
        -> Y.CoroutineT
            (Either (LuaValue q s) [LuaValue q s])
            [LuaValue q s]
            (LuaValue q s)
            (LuaHost s)
            [LuaValue q s]))
    | LTNormal
    | LTDead


type LuaThread q s = LuaRef q s (LuaThreadState q s)


newtype LuaPure t q s = LuaPure t deriving (Show)


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
errDivideZero = LString $ "Attempt to divide by zero"

errNilIndex :: LuaValue q s
errNilIndex = LString $ "Attempt to index a table with a nil"

errNonSuspended :: LuaValue q s
errNonSuspended = LString $ "Attempt to resume a non-suspended coroutine"

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


data LuaMetatypeWrapper q s where
    LuaMetatypeWrapper
        :: (   forall u . (forall t . LuaMetatype t => t q s -> u)
            -> LuaValue q s -> u)
        -> LuaMetatypeWrapper q s


data LuaEnvironment q s = LuaEnvironment {
    lenvCounter
        :: LuaRef q s Int,
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
        :: LuaRef q s (LuaValue q s, Bool, Bool, Bool)}


type LuaErrorHandler q s = LuaValue q s -> LuaState q s (LuaValue q s)


type LuaVariableList q s = [(
    Maybe (SourceRange, BSt.ByteString),
    Either (LuaValue q s) (LuaRef q s (LuaValue q s)))]


data LuaStackFrame q s = LuaStackFrame {
    lsfDefinition :: Maybe (SourceRange, BSt.ByteString),
    lsfCurrentLocation :: LuaRef q s (Maybe SourceRange),
    lsfUpvalues :: LuaVariableList q s,
    lsfLocals :: LuaVariableList q s}


lsfModifyLocals
    :: (LuaVariableList q s -> LuaVariableList q s)
    -> LuaStackFrame q s
    -> LuaStackFrame q s
lsfModifyLocals f (LuaStackFrame d c u l)
    = LuaStackFrame d c u (f l)


data LuaContext q s = LuaContext {
    lctxEnvironment :: LuaEnvironment q s,
    lctxErrHandler :: LuaErrorHandler q s,
    lctxCurrentThread :: LuaThread q s,
    lctxStack :: [LuaStackFrame q s]}


lctxModifyErrHandler
    :: (LuaErrorHandler q s -> LuaErrorHandler q s)
    -> LuaContext q s
    -> LuaContext q s
lctxModifyErrHandler f (LuaContext e h t s) = LuaContext e (f h) t s


lctxModifyStack
    :: ([LuaStackFrame q s] -> [LuaStackFrame q s])
    -> LuaContext q s
    -> LuaContext q s
lctxModifyStack f (LuaContext e h t s) = LuaContext e h t (f s)


newtype LuaState q s t = LuaState {
    runLuaState
        :: ReaderT
            (LuaContext q s)
            (Y.CoroutineT
                (Either (LuaValue q s) [LuaValue q s])
                [LuaValue q s]
                (LuaValue q s)
                (LuaHost s))
            t}


lxRunT
    :: (Monad m)
    => (forall a . ST s a -> m a)
    -> (forall a . IO a -> m (Maybe a))
    -> (forall q . [LuaValue q s] -> m (Either (LuaValue q s) [LuaValue q s]))
    -> (forall q . LuaValue q s -> m u)
    -> (t -> m u)
    -> (forall q . LuaState q s t)
    -> m u
lxRunT resolveST resolveIO resolveYield onError onPure (LuaState lstate) = do
    env <- resolveST $ LuaEnvironment
        <$> newSTRef 0
        <*> newSTRef lxDefaultMetatable
        <*> newSTRef lxDefaultMetatable
        <*> newSTRef lxDefaultMetatable
        <*> newSTRef lxDefaultMetatable
        <*> newSTRef lxDefaultMetatable
        <*> newSTRef lxDefaultMetatable
        <*> (newSTRef =<< makeDefaultWarnHandler)
        <*> newSTRef (LNil, False, False, False)
    thread <- resolveST $ newSTRef LTRunning
    driveY $ runReaderT lstate (LuaContext env return thread [])
    where
    driveY (Y.Pure x) = onPure x
    driveY (Y.Error e) = onError e
    driveY (Y.Hold vals g) = (driveY . g) =<< resolveYield vals
    driveY (Y.Lift hostM) = driveHost $ hostM
    driveHost (LuaHostPure y) = driveY $ y
    driveHost (LuaHostLiftedST act next) = (driveHost . next) =<< resolveST act
    driveHost (LuaHostLiftedIO act next) = (driveHost . next) =<< resolveIO act


instance MonadReader (LuaContext q s) (LuaState q s) where
    ask = LuaState $ ask
    local f act = LuaState $ local f $ runLuaState act
    reader f = LuaState $ reader f


instance Monad (LuaState q s) where
    return x = LuaState $ return x
    LuaState st >>= f = LuaState $ st >>= (runLuaState . f)


instance Functor (LuaState q s) where
    fmap f (LuaState st) = LuaState $ fmap f st


instance Applicative (LuaState q s) where
    pure x = LuaState $ pure x
    LuaState sta <*> LuaState stb = LuaState $ sta <*> stb


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


lxYield :: [LuaValue q s] -> LuaState q s [LuaValue q s]
lxYield bvals = do
    result <- LuaState $ lift $ Y.yield bvals
    case result of
        Left err -> lxError $ err
        Right avals -> return $ avals


lxError :: LuaValue q s -> LuaState q s a
lxError err = do
    errh <- lctxErrHandler <$> ask
    err' <- local (lctxModifyErrHandler (\_ -> return)) $ errh err
    LuaState $ lift $ Y.raise err'


lxTry :: LuaState q s t -> LuaState q s (Either (LuaValue q s) t)
lxTry (LuaState act) = LuaState $ mapReaderT Y.try $ act


lxWithErrHandler
    :: LuaErrorHandler q s
    -> LuaState q s t
    -> LuaState q s t
lxWithErrHandler errh act = local (lctxModifyErrHandler (\_ -> errh)) $ act


lxWarn :: LuaValue q s -> LuaState q s ()
lxWarn e = do
    env <- lctxEnvironment <$> ask
    warnh <- lxRead $ lenvWarnHandler env
    _ <- lxTry (warnh e)
    return ()


lxFinally :: LuaState q s () -> LuaState q s t -> LuaState q s t
lxFinally fin act = do
    ar <- lxTry act
    fr <- lxTry fin
    case ar of
        Left aerr -> do
            case fr of
                Left ferr -> lxWarn ferr
                Right _ -> return $ ()
            LuaState $ lift $ Y.raise aerr
        Right asuc -> do
            case fr of
                Left ferr -> LuaState $ lift $ Y.raise ferr
                Right _ -> return $ asuc


lxLiftST :: ST s a -> LuaState q s a
lxLiftST !act = LuaState $! lift $! lift $! lhostLiftST $! act


lxTryLiftIO :: IO a -> LuaState q s (Maybe a)
lxTryLiftIO !act = LuaState $! lift $! lift $! lhostLiftIO $! act


lxAlloc :: a -> LuaState q s (LuaRef q s a)
lxAlloc x = lxLiftST $ newSTRef x


lxRead :: LuaRef q s a -> LuaState q s a
lxRead ref = lxLiftST $ readSTRef ref


lxWrite :: LuaRef q s a -> a -> LuaState q s ()
lxWrite ref !x = lxLiftST $ writeSTRef ref x


lxModify :: LuaRef q s a -> (a -> a) -> LuaState q s ()
lxModify ref f = lxLiftST $ modifySTRef' ref f


lxNewThread
    :: LuaFunction q s
    -> LuaState q s (LuaThread q s)
lxNewThread body = do
    env <- lctxEnvironment <$> ask
    thread <- lxAlloc $ LTDead
    let start seed = (do
        case seed of
            Left err -> Y.Error err
            Right args -> do
                let (LuaState rm) = body args
                let context = LuaContext env return thread []
                runReaderT rm context)
    lxWrite thread $ LTSuspended start
    return $ thread


lxResume
    :: LuaThread q s
    -> Either (LuaValue q s) [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
lxResume thread ivals = do
    ts <- lxRead thread
    case ts of
        LTSuspended f -> do
            current <- lctxCurrentThread <$> ask
            lxWrite current $ LTNormal
            lxWrite thread $ LTRunning
            (result, rtstate) <- driveY (f ivals)
            lxWrite thread $ rtstate
            lxWrite current $ LTRunning
            return $ result
        _ -> return $ Left errNonSuspended
    where
    driveY (Y.Pure x) = return $ (Right x, LTDead)
    driveY (Y.Error e) = return $ (Left e, LTDead)
    driveY (Y.Hold vals g) = return $ (Right vals, LTSuspended g)
    driveY (Y.Lift hostM) = driveHost $ hostM
    driveHost (LuaHostPure y) = driveY $ y
    driveHost (LuaHostLiftedST act next) =
        (driveHost . next) =<< lxLiftST act
    driveHost (LuaHostLiftedIO act next) =
        (driveHost . next) =<< lxTryLiftIO act


lxThreadState
    :: LuaThread q s
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
    -> LuaState q s t
lxThreadState thread onRunning onSuspended onNormal onDead = do
    state <- lxRead thread
    case state of
        LTRunning -> onRunning
        LTSuspended _ -> onSuspended
        LTNormal -> onNormal
        LTDead -> onDead


lxNewId :: LuaState q s LuaId
lxNewId = do
    ref <- lenvCounter . lctxEnvironment <$> ask
    lxModify ref (+1)
    LuaId <$> lxRead ref


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
    body <- lxLiftST $ do
        body' <- T.new
        flip fix elems $ \loop elems1 -> do
            case elems1 of
                (_, LNil):rest -> loop rest
                (!kval, !vval):rest -> do
                    T.set body' (lxKey kval) (kval, vval)
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
    env <- lctxEnvironment <$> ask
    lxWrite (lenvNilMetatable env) mt


lxSetBoolMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetBoolMetatable mt = do
    env <- lctxEnvironment <$> ask
    lxWrite (lenvBoolMetatable env) mt


lxSetNumberMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetNumberMetatable mt = do
    env <- lctxEnvironment <$> ask
    lxWrite (lenvNumberMetatable env) mt


lxSetStringMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetStringMetatable mt = do
    env <- lctxEnvironment <$> ask
    lxWrite (lenvStringMetatable env) mt


lxSetFunctionMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetFunctionMetatable mt = do
    env <- lctxEnvironment <$> ask
    lxWrite (lenvFunctionMetatable env) mt


lxSetThreadMetatable
    :: LuaMetatypeWrapper q s
    -> LuaState q s ()
lxSetThreadMetatable mt = do
    env <- lctxEnvironment <$> ask
    lxWrite (lenvThreadMetatable env) mt


lxSetWarnHandler
    :: (LuaValue q s -> LuaState q s ())
    -> LuaState q s ()
lxSetWarnHandler wh = do
    env <- lctxEnvironment <$> ask
    lxWrite (lenvWarnHandler env) wh


lxGetDebugHook
    :: LuaState q s (LuaValue q s, Bool, Bool, Bool)
lxGetDebugHook = do
    env <- lctxEnvironment <$> ask
    lxRead (lenvDebugHook env)


lxSetDebugHook
    :: (LuaValue q s, Bool, Bool, Bool)
    -> LuaState q s ()
lxSetDebugHook dh = do
    env <- lctxEnvironment <$> ask
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
        state <- lctxEnvironment <$> ask
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


lxRawArithUnm
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithUnm a def = do
    case a of
        LInteger x  -> return $ LInteger  $ negate x
        LRational x -> return $ LRational $ negate x
        LDouble x   -> return $ LDouble   $ negate x
        _ -> def


lxRawArithBNot
    :: LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithBNot a def = do
    case a of
        LInteger x  -> LInteger <$> (complement <$> cvInt x)
        LRational x -> LInteger <$> (complement <$> cvRat x)
        LDouble x   -> LInteger <$> (complement <$> cvDbl x)
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
    case (a, b) of
        (LInteger  x, LInteger  y) -> return $ LInteger  $ op x y
        (LInteger  x, LRational y) -> return $ LRational $ op (fromInteger x) y
        (LInteger  x, LDouble   y) -> return $ LDouble   $ op (fromInteger x) y
        (LRational x, LInteger  y) -> return $ LRational $ op x (fromInteger y)
        (LRational x, LRational y) -> return $ LRational $ op x y
        (LRational x, LDouble   y) -> return $ LDouble   $ op (fromRational x) y
        (LDouble   x, LInteger  y) -> return $ LDouble   $ op x (fromInteger y)
        (LDouble   x, LRational y) -> return $ LDouble   $ op x (fromRational y)
        (LDouble   x, LDouble   y) -> return $ LDouble   $ op x y
        _ -> def


lxRawArithDiv
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
    -> LuaState q s (LuaValue q s)
lxRawArithDiv a b def = do
    case (a, b) of
        (LInteger  x, LInteger  y) -> doInteger x y
        (LInteger  x, LRational y) -> doRational (fromInteger x) y
        (LInteger  x, LDouble   y) -> doDouble (fromInteger x) y
        (LRational x, LInteger  y) -> doRational x (fromInteger y)
        (LRational x, LRational y) -> doRational x y
        (LRational x, LDouble   y) -> doDouble (fromRational x) y
        (LDouble   x, LInteger  y) -> doDouble x (fromInteger y)
        (LDouble   x, LRational y) -> doDouble x (fromRational y)
        (LDouble   x, LDouble   y) -> doDouble x y
        _ -> def
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
    case (a, b) of
        (LInteger  x, LInteger  y) -> doInteger x y
        (LInteger  x, LRational y) -> doRational (fromInteger x) y
        (LInteger  x, LDouble   y) -> doDouble (fromInteger x) y
        (LRational x, LInteger  y) -> doRational x (fromInteger y)
        (LRational x, LRational y) -> doRational x y
        (LRational x, LDouble   y) -> doDouble (fromRational x) y
        (LDouble   x, LInteger  y) -> doDouble x (fromInteger y)
        (LDouble   x, LRational y) -> doDouble x (fromRational y)
        (LDouble   x, LDouble   y) -> doDouble x y
        _ -> def
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
    case (a, b) of
        (LInteger  x, LInteger  y) -> doInteger x y
        (LInteger  x, LRational y) -> doRational (fromInteger x) y
        (LInteger  x, LDouble   y) -> doDouble (fromInteger x) y
        (LRational x, LInteger  y) -> doRational x (fromInteger y)
        (LRational x, LRational y) -> doRational x y
        (LRational x, LDouble   y) -> doDouble (fromRational x) y
        (LDouble   x, LInteger  y) -> doDouble x (fromInteger y)
        (LDouble   x, LRational y) -> doDouble x (fromRational y)
        (LDouble   x, LDouble   y) -> doDouble x y
        _ -> def
    where
    doInteger x y =
        if y == 0
            then lxError $ errDivideZero
            else return $ LInteger $ mod x y
    doRational x y =
        if y == 0
            then doDouble (fromRational x) (fromRational y)
            else return $ LRational $ x - y * fromInteger (floor (x / y))
    doDouble x y =
        return $ LDouble $
            if isInfinite y
                then if y > 0
                    then x
                    else negate x
                else let r = x / y in
                    if isNaN r || isInfinite r
                        then (0/0)
                        else x - y * fromInteger (floor r)


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
        (LDouble   x, LInteger  y) -> return $ doRatDbl (fromInteger y) x
        (LDouble   x, LRational y) -> return $ doRatDbl y x
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
    lctxStack <$> ask


lxLocalStack
    :: ([LuaStackFrame q s] -> [LuaStackFrame q s])
    -> LuaState q s t
    -> LuaState q s t
lxLocalStack func act = do
    local (lctxModifyStack func) act
