{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


module Lua.Core (
    LuaErrorHandler,
    LuaFunction,
    LuaMetatable(..),
    LuaPure(..),
    LuaRef,
    LuaStackFrame(..),
    LuaState,
    LuaTableBody(..),
    LuaValue(..),
    LuaVariableList,
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
    lsfModifyLocals,
    lvalTypename,
    lxAlloc,
    lxAllocTable,
    lxAsString,
    lxAskStack,
    lxCall,
    lxCar,
    lxError,
    lxExtend,
    lxFinally,
    lxGet,
    lxGetTable,
    lxKey,
    lxLiftST,
    lxLocalStack,
    lxMetatable,
    lxNewId,
    lxNewThread,
    lxPerformBinary,
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
    lxRawConcat,
    lxRawLen,
    lxRead,
    lxResume,
    lxRunT,
    lxSet,
    lxSetTable,
    lxTableLength,
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
import Data.String
import Data.STRef
import Type.Reflection
import qualified Data.ByteString.Char8 as BSt
import qualified Data.Map.Strict as M
import Lua.SourceRange
import qualified Lua.Coroutine as Y


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


type LuaMetaopUnary r q s t = Maybe (t q s -> LuaState q s r)


type LuaMetaopBinary r q s t = Maybe (
    t q s -> LuaValue q s -> LuaState q s r,
    LuaValue q s -> t q s -> LuaState q s r)


data LuaMetatable q s t = LuaMetatable {
    lmtTypify :: forall a . ((Typeable t) => t q s -> a) -> t q s -> a,
    lmtMetatable :: LuaValue q s,
    lmtTypename :: (forall a . (IsString a) => Maybe a),
    lmtAdd :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtSub :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtMul :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtDiv :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtMod :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtPow :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtUnm :: LuaMetaopUnary (LuaValue q s) q s t,
    lmtIDiv :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtBAnd :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtBOr :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtBXor :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtBNot :: LuaMetaopUnary (LuaValue q s) q s t,
    lmtShl :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtShr :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtConcat :: LuaMetaopBinary (LuaValue q s) q s t,
    lmtLen :: LuaMetaopUnary (LuaValue q s) q s t,
    lmtEq :: LuaMetaopBinary Bool q s t,
    lmtLt :: LuaMetaopBinary Bool q s t,
    lmtLe :: LuaMetaopBinary Bool q s t,
    lmtIndex
        :: Maybe
            (  t q s
            -> LuaValue q s
            -> LuaState q s (LuaValue q s)),
    lmtNewIndex
        :: Maybe
            (  t q s
            -> LuaValue q s
            -> LuaValue q s
            -> LuaState q s ()),
    lmtCall
        :: Maybe
            (  t q s
            -> [LuaValue q s]
            -> LuaState q s [LuaValue q s]),
    lmtClose
        :: Maybe
            (  t q s
            -> LuaState q s ()),
    lmtToString
        :: Maybe
            (  t q s
            -> LuaState q s BSt.ByteString)}


lvalEmptyMetatable :: (Typeable t) => LuaMetatable q s t
lvalEmptyMetatable
    = LuaMetatable
        ($)
        LNil
        Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


newtype LuaId = LuaId Int deriving (Show, Eq, Ord)


data LuaTableBody q s = LuaTableBody {
    ltbItems :: M.Map (LuaKey q s) (LuaValue q s),
    ltbMetatable :: LuaMetatable q s LuaValue}


type LuaTable q s = LuaRef q s (LuaTableBody q s)


type LuaFunction q s = [LuaValue q s] -> LuaState q s [LuaValue q s]


data LuaThreadState q s
    = LTRunning
    | LTSuspended
        (Either (LuaValue q s) [LuaValue q s]
        -> Y.CoroutineT
            (Either (LuaValue q s) [LuaValue q s])
            [LuaValue q s]
            (LuaValue q s)
            (LuaHost s)
            [LuaValue q s])
    | LTNormal
    | LTDead


type LuaThread q s = LuaRef q s (LuaThreadState q s)


newtype LuaPure t q s = LuaPure t deriving (Show)


data LuaValue q s where
    LNil :: LuaValue q s
    LBool :: Bool -> LuaValue q s
    LInteger :: Integer -> LuaValue q s
    LRational :: Rational -> LuaValue q s
    LDouble :: Double -> LuaValue q s
    LString :: BSt.ByteString -> LuaValue q s
    LFunction
        :: LuaId
        -> LuaFunction q s
        -> LuaValue q s
    LThread
        :: LuaId
        -> LuaThread q s
        -> LuaValue q s
    LTable
        :: LuaId
        -> LuaTable q s
        -> LuaValue q s
    LUserdata
        :: LuaId
        -> t q s
        -> LuaMetatable q s t
        -> LuaValue q s


instance Show (LuaValue q s) where
    show LNil = "nil"
    show (LBool False) = "false"
    show (LBool True) = "true"
    show (LInteger i) = show i
    show (LRational r) = show (numerator r) ++ "/" ++ show (denominator r)
    show (LDouble d) = show d
    show (LString s) = "\"" ++ qstr (BSt.unpack s) ++ "\""
        where
        qstr "" = ""
        qstr (c:cs)
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
        dstart (c:cs) = '0' <= c && c <= '9'
    show (LFunction (LuaId i) _) = "<function #" ++ show i ++ ">"
    show (LThread (LuaId i) _) = "<thread #" ++ show i ++ ">"
    show (LTable (LuaId i) _) = "<table #" ++ show i ++ ">"
    show (LUserdata (LuaId i) _ mt)
        = "<" ++ fromMaybe "userdata" (lmtTypename mt)
        ++ " #" ++ show i ++ ">"


lvalTypename :: (IsString a) => LuaValue q s -> a
lvalTypename LNil = "nil"
lvalTypename (LBool False) = "boolean"
lvalTypename (LBool True) = "boolean"
lvalTypename (LInteger i) = "number"
lvalTypename (LRational r) = "number"
lvalTypename (LDouble d) = "number"
lvalTypename (LString s) = "string"
lvalTypename (LFunction _ _) = "function"
lvalTypename (LThread _ _) = "thread"
lvalTypename (LTable _ _) = "table"
lvalTypename (LUserdata _ _ mt) = fromMaybe "userdata" (lmtTypename mt)


data LuaKey q s
    = LKInteger Integer
    | LKNil
    | LKBool Bool
    | LKRational Rational
    | LKDouble Double
    | LKString BSt.ByteString
    | LKObject LuaId
    deriving (Show, Ord, Eq)


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

errWrongYield :: LuaValue q s
errWrongYield = LString "Attempt to yield in a non-yieldable context"

errZeroStep :: LuaValue q s
errZeroStep = LString "Attempt to perform a ranged for loop with step zero"


data LuaEnvironment q s = LuaEnvironment {
    lenvCounter :: LuaRef q s Int,
    lenvNilMetatable :: LuaRef q s (LuaMetatable q s LuaValue),
    lenvBoolMetatable :: LuaRef q s (LuaMetatable q s LuaValue),
    lenvNumberMetatable :: LuaRef q s (LuaMetatable q s LuaValue),
    lenvStringMetatable :: LuaRef q s (LuaMetatable q s LuaValue),
    lenvFunctionMetatable :: LuaRef q s (LuaMetatable q s LuaValue),
    lenvThreadMetatable :: LuaRef q s (LuaMetatable q s LuaValue),
    lenvWarnHandler :: LuaRef q s (LuaValue q s -> LuaState q s ())}


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
lxRunT resolveST resolveIO resolveYield onError onPure (LuaState act) = do
    env <- resolveST $ LuaEnvironment
        <$> newSTRef 0
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> (newSTRef =<< makeDefaultWarnHandler)
    thread <- resolveST $ newSTRef LTRunning
    driveY $ runReaderT act (LuaContext env return thread [])
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
                    lxTryLiftIO $ putStrLn $
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
    lxTry (warnh e)
    return ()


lxFinally :: LuaState q s () -> LuaState q s t -> LuaState q s t
lxFinally fin act = do
    ar <- lxTry act
    fr <- lxTry fin
    case ar of
        Left ae -> do
            case fr of
                Left fe -> lxWarn fe
                Right _ -> return $ ()
            LuaState $ lift $ Y.raise ae
        Right ar -> do
            case fr of
                Left fe -> LuaState $ lift $ Y.raise fe
                Right _ -> return $ ar


lxLiftST :: ST s a -> LuaState q s a
lxLiftST act = LuaState $ lift $ lift $ lhostLiftST $ act


lxTryLiftIO :: IO a -> LuaState q s (Maybe a)
lxTryLiftIO act = LuaState $ lift $ lift $ lhostLiftIO $ act


lxAlloc :: a -> LuaState q s (LuaRef q s a)
lxAlloc x = lxLiftST $ newSTRef x


lxRead :: LuaRef q s a -> LuaState q s a
lxRead ref = lxLiftST $ readSTRef ref


lxWrite :: LuaRef q s a -> a -> LuaState q s ()
lxWrite ref x = lxLiftST $ writeSTRef ref x


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


lxNewId :: LuaState q s LuaId
lxNewId = do
    ref <- lenvCounter . lctxEnvironment <$> ask
    lxModify ref (+1)
    LuaId <$> lxRead ref


lxKey :: LuaValue q s -> LuaState q s (LuaKey q s)
lxKey LNil = return $ LKNil
lxKey (LBool b) = return $ LKBool b
lxKey (LInteger i) = return $ LKInteger $ i
lxKey (LRational r) =
    if denominator r == 1
        then return $ LKInteger $ numerator r
        else return $ LKRational $ r
lxKey (LDouble d) =
    if isNaN d || isInfinite d
        then return $ LKDouble d
        else lxKey $ LRational $ toRational d
lxKey (LString str) = return $ LKString $ str
lxKey (LFunction i _) = return $ LKObject $ i
lxKey (LThread i _) = return $ LKObject $ i
lxKey (LTable i _ ) = return $ LKObject $ i
lxKey (LUserdata i _ _) = return $ LKObject $ i


lxAllocTable
    :: [(LuaKey q s, LuaValue q s)]
    -> LuaState q s (LuaTable q s)
lxAllocTable elems = do
    lxAlloc $ LuaTableBody (M.fromList elems) lvalEmptyMetatable


lxGetTable
    :: LuaKey q s
    -> LuaTable q s
    -> LuaState q s (Maybe (LuaValue q s))
lxGetTable key table = do
    LuaTableBody items mt <- lxRead table
    return $ M.lookup key items


lxSetTable
    :: LuaKey q s
    -> LuaValue q s
    -> LuaTable q s
    -> LuaState q s ()
lxSetTable key LNil table = do
    LuaTableBody items mt <- lxRead table
    let items' = M.delete key items
    lxWrite table $ LuaTableBody items' mt
lxSetTable key value table = do
    LuaTableBody items mt <- lxRead table
    let items' = M.insert key value items
    lxWrite table $ LuaTableBody items' mt


lxTableLength
    :: LuaTable q s
    -> LuaState q s Integer
lxTableLength table = do
    LuaTableBody items mt <- lxRead table
    case M.lookupLT LKNil items of
        Just (ltk, _) ->
            case ltk of
                LKInteger x ->
                    if x > 0
                        then return $ x
                        else return $ 0
                _ -> return $ 0
        Nothing -> return $ 0


lxExtend :: Int -> [LuaValue q s] -> [LuaValue q s]
lxExtend 0 xs = xs
lxExtend n [] = LNil:lxExtend (n-1) []
lxExtend n (x:xs) = x:lxExtend (n-1) xs


lxCar :: [LuaValue q s] -> LuaValue q s
lxCar [] = LNil
lxCar (a:rest) = a


lxProduceMetatable
    :: LuaValue q s
    -> LuaState q s (LuaMetatable q s LuaValue)
lxProduceMetatable tvalue@(LTable _ table) = do
    LuaTableBody items mt <- lxRead table
    let metatable = LuaMetatable {
        lmtTypify = ($),
        lmtMetatable = tvalue,
        lmtTypename = Nothing,
        lmtAdd = mkMetaopBinary <$!> M.lookup (LKString "__add") items,
        lmtSub = mkMetaopBinary <$!> M.lookup (LKString "__sub") items,
        lmtMul = mkMetaopBinary <$!> M.lookup (LKString "__mul") items,
        lmtDiv = mkMetaopBinary <$!> M.lookup (LKString "__div") items,
        lmtMod = mkMetaopBinary <$!> M.lookup (LKString "__mod") items,
        lmtPow = mkMetaopBinary <$!> M.lookup (LKString "__pow") items,
        lmtUnm = mkMetaopUnary <$!> M.lookup (LKString "__unm") items,
        lmtIDiv = mkMetaopBinary <$!> M.lookup (LKString "__idiv") items,
        lmtBAnd = mkMetaopBinary <$!> M.lookup (LKString "__band") items,
        lmtBOr = mkMetaopBinary <$!> M.lookup (LKString "__bor") items,
        lmtBXor = mkMetaopBinary <$!> M.lookup (LKString "__bxor") items,
        lmtBNot = mkMetaopUnary <$!> M.lookup (LKString "__bnot") items,
        lmtShl = mkMetaopBinary <$!> M.lookup (LKString "__shl") items,
        lmtShr = mkMetaopBinary <$!> M.lookup (LKString "__shr") items,
        lmtConcat = mkMetaopBinary <$!> M.lookup (LKString "__concat") items,
        lmtLen = mkMetaopUnary <$!> M.lookup (LKString "__len") items,
        lmtEq = mkMetaopBinaryBool <$!> M.lookup (LKString "__eq") items,
        lmtLt = mkMetaopBinaryBool <$!> M.lookup (LKString "__lt") items,
        lmtLe = mkMetaopBinaryBool <$!> M.lookup (LKString "__le") items,
        lmtIndex = mkMetaopIndex <$!> M.lookup (LKString "__index") items,
        lmtNewIndex =
            mkMetaopNewIndex <$!> M.lookup (LKString "__newindex") items,
        lmtCall = mkMetaopCall <$!> M.lookup (LKString "__call") items,
        lmtClose = mkMetaopClose <$!> M.lookup (LKString "__close") items,
        lmtToString = mkMetaopStr <$!> M.lookup (LKString "__tostring") items}
    return $ metatable
    where
    mkMetaopUnary f = \a -> lxCar <$> lxCall f [a]
    mkMetaopBinary f = (
        \a b -> lxCar <$> lxCall f [a, b],
        \a b -> lxCar <$> lxCall f [a, b])
    mkMetaopBinaryBool f = (
        \a b -> listToBool <$> lxCall f [a, b],
        \a b -> listToBool <$> lxCall f [a, b])
    listToBool (LNil:_) = False
    listToBool (LBool False:_) = False
    listToBool (_:_) = True
    listToBool _ = False
    mkMetaopIndex (LFunction _ ff) = \a b -> lxCar <$> ff [a, b]
    mkMetaopIndex f = \a b -> lxGet f b
    mkMetaopNewIndex (LFunction _ ff) = \a b c -> () <$ ff [a, b, c]
    mkMetaopNewIndex f = \a b c -> lxSet f b c
    mkMetaopCall f = \a args -> lxCall f (a:args)
    mkMetaopClose f = \a -> () <$ lxCall f [a]
    mkMetaopString (LFunction _ ff) = \a -> lxAsString =<< (lxCar <$> ff [a])
    mkMetaopStr f = \a -> lxAsString a
lxProduceMetatable _ = return $ lvalEmptyMetatable


lxMetatable
    :: LuaValue q s
    -> (forall t . (Typeable t)
        => t q s
        -> LuaMetatable q s t
        -> LuaState q s a)
    -> LuaState q s a
lxMetatable x f = do
    state <- lctxEnvironment <$> ask
    case x of
        LNil -> f x =<< (lxRead $ lenvNilMetatable state)
        LBool _ -> f x =<< (lxRead $ lenvBoolMetatable state)
        LInteger _ -> f x =<< (lxRead $ lenvNumberMetatable state)
        LRational _ -> f x =<< (lxRead $ lenvNumberMetatable state)
        LDouble _ -> f x =<< (lxRead $ lenvNumberMetatable state)
        LString _ -> f x =<< (lxRead $ lenvStringMetatable state)
        LFunction _ _ -> f x =<< (lxRead $ lenvFunctionMetatable state)
        LThread _ _ -> f x =<< (lxRead $ lenvThreadMetatable state)
        LTable _ table -> f x =<< (ltbMetatable <$> lxRead table)
        LUserdata _ ud mt -> lmtTypify mt f ud mt


lxPerformMetaopUnary
    :: (forall t . LuaMetatable q s t -> LuaMetaopUnary r q s t)
    -> LuaValue q s
    -> LuaState q s (Maybe r)
lxPerformMetaopUnary metaf a = do
    lxMetatable a (\x mt ->
        case metaf mt of
            Just metaop -> Just <$> metaop x
            Nothing -> return $ Nothing)


lxPerformMetaopBinary
    :: (forall t . LuaMetatable q s t -> LuaMetaopBinary r q s t)
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s (Maybe r)
lxPerformMetaopBinary metaf a b = do
    lxMetatable a (\x mta ->
        lxMetatable b (\y mtb -> do
            case metaf mta of
                Just (ma, _) -> Just <$> ma x b
                Nothing ->
                    case metaf mtb of
                        Just (_, mb) -> Just <$> mb a y
                        Nothing -> return $ Nothing))


lxPerformUnary
    :: (LuaValue q s -> LuaState q s (Maybe r))
    -> (forall t . LuaMetatable q s t -> LuaMetaopUnary r q s t)
    -> LuaState q s r
    -> LuaValue q s
    -> LuaState q s r
lxPerformUnary raw metaf def a = do
    rr <- raw a
    case rr of
        Just ret -> return $ ret
        Nothing -> do
            mr <- lxPerformMetaopUnary metaf a
            case mr of
                Just ret' -> return $ ret'
                Nothing -> def


lxPerformBinary
    :: (LuaValue q s -> LuaValue q s -> LuaState q s (Maybe r))
    -> (forall t . LuaMetatable q s t -> LuaMetaopBinary r q s t)
    -> LuaState q s r
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s r
lxPerformBinary raw metaf def a b = do
    rr <- raw a b
    case rr of
        Just ret -> return $ ret
        Nothing -> do
            mr <- lxPerformMetaopBinary metaf a b
            case mr of
                Just ret' -> return $ ret'
                Nothing -> def


lxRawArithUnm
    :: LuaValue q s
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithUnm a = do
    case a of
        LInteger x  -> return $ Just $ LInteger  $ negate x
        LRational x -> return $ Just $ LRational $ negate x
        LDouble x   -> return $ Just $ LDouble   $ negate x
        _ -> return $ Nothing


lxRawArithBNot
    :: LuaValue q s
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithBNot a = do
    case a of
        LInteger x  -> (Just . LInteger) <$> (complement <$> cvInt x)
        LRational x -> (Just . LInteger) <$> (complement <$> cvRat x)
        LDouble x   -> (Just . LInteger) <$> (complement <$> cvDbl x)
        _ -> return $ Nothing
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
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithNum op a b = do
    return $ case (a, b) of
        (LInteger  x, LInteger  y) -> Just $ LInteger  $ op x y
        (LInteger  x, LRational y) -> Just $ LRational $ op (fromInteger x) y
        (LInteger  x, LDouble   y) -> Just $ LDouble   $ op (fromInteger x) y
        (LRational x, LInteger  y) -> Just $ LRational $ op x (fromInteger y)
        (LRational x, LRational y) -> Just $ LRational $ op x y
        (LRational x, LDouble   y) -> Just $ LDouble   $ op (fromRational x) y
        (LDouble   x, LInteger  y) -> Just $ LDouble   $ op x (fromInteger y)
        (LDouble   x, LRational y) -> Just $ LDouble   $ op x (fromRational y)
        (LDouble   x, LDouble   y) -> Just $ LDouble   $ op x y
        _ -> Nothing


lxRawArithDiv
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithDiv a b = do
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
        _ -> return $ Nothing
    where
    doInteger x y =
        if y == 0
            then doDouble (fromInteger x) (fromInteger y)
            else return $ Just $ LRational $ (fromInteger x) / (fromInteger y)
    doRational x y =
        if y == 0
            then doDouble (fromRational x) (fromRational y)
            else return $ Just $ LRational $ x / y
    doDouble x y =
        return $ Just $ LDouble $ x / y


lxRawArithIDiv
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithIDiv a b = do
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
        _ -> return $ Nothing
    where
    doInteger x y =
        if y == 0
            then lxError $ errDivideZero
            else return $ Just $ LInteger $ div x y
    doRational x y =
        if y == 0
            then doDouble (fromRational x) (fromRational y)
            else return $ Just $ LRational $ fromInteger (floor (x / y))
    doDouble x y =
        return $ Just $ LDouble $ let r = x / y in
            if isNaN r || isInfinite r || isNegativeZero r
                then r
                else fromInteger (floor r)


lxRawArithMod
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithMod a b = do
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
        _ -> return $ Nothing
    where
    doInteger x y =
        if y == 0
            then lxError $ errDivideZero
            else return $ Just $ LInteger $ mod x y
    doRational x y =
        if y == 0
            then doDouble (fromRational x) (fromRational y)
            else return $ Just $ LRational $ x - y * fromInteger (floor (x / y))
    doDouble x y =
        return $ Just $ LDouble $
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
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithPow a b = do
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
        _ -> return $ Nothing
    where
    doDouble x y =
        return $ Just $ LDouble $ x ** y


lxRawArithBitwise
    :: (Integer -> Integer -> Integer)
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s (Maybe (LuaValue q s))
lxRawArithBitwise op a b = do
    case (a, b) of
        (LInteger  x, LInteger  y) ->
            (Just . LInteger) <$> (op <$> (cvInt x) <*> (cvInt y))
        (LInteger  x, LRational y) ->
            (Just . LInteger) <$> (op <$> (cvInt x) <*> (cvRat y))
        (LInteger  x, LDouble   y) ->
            (Just . LInteger) <$> (op <$> (cvInt x) <*> (cvDbl y))
        (LRational x, LInteger  y) ->
            (Just . LInteger) <$> (op <$> (cvRat x) <*> (cvInt y))
        (LRational x, LRational y) ->
            (Just . LInteger) <$> (op <$> (cvRat x) <*> (cvRat y))
        (LRational x, LDouble   y) ->
            (Just . LInteger) <$> (op <$> (cvRat x) <*> (cvDbl y))
        (LDouble   x, LInteger  y) ->
            (Just . LInteger) <$> (op <$> (cvDbl x) <*> (cvInt y))
        (LDouble   x, LRational y) ->
            (Just . LInteger) <$> (op <$> (cvDbl x) <*> (cvRat y))
        (LDouble   x, LDouble   y) ->
            (Just . LInteger) <$> (op <$> (cvDbl x) <*> (cvDbl y))
        _ -> return $ Nothing
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
    -> LuaState q s (Maybe Bool)
lxRawCompare op a b = do
    case (a, b) of
        (LInteger  x, LInteger  y) -> return $ Just $ op x y
        (LInteger  x, LRational y) -> return $ Just $ op (fromInteger x) y
        (LInteger  x, LDouble   y) -> return $ Just $ doRatDbl (fromInteger x) y
        (LRational x, LInteger  y) -> return $ Just $ op x (fromInteger y)
        (LRational x, LRational y) -> return $ Just $ op x y
        (LRational x, LDouble   y) -> return $ Just $ doRatDbl x y
        (LDouble   x, LInteger  y) -> return $ Just $ doDblRat x (fromInteger y)
        (LDouble   x, LRational y) -> return $ Just $ doDblRat x y
        (LDouble   x, LDouble   y) -> return $ Just $ op x y
        (LString x, LString y) -> return $ Just $ op x y
        _ -> return $ Nothing
    where
    doRatDbl x y =
        if isNaN y || isInfinite y
            then op (fromRational x) y
            else op x (toRational y)
    doDblRat x y =
        if isNaN x || isInfinite x
            then op x (fromRational y)
            else op (toRational x) y


lxRawCompareEq
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (Maybe Bool)
lxRawCompareEq a b = do
    case (a, b) of
        (LInteger  x, LInteger  y) -> return $ Just $ x == y
        (LInteger  x, LRational y) -> return $ Just $ (fromInteger x) == y
        (LInteger  x, LDouble   y) -> return $ Just $ doRatDbl (fromInteger x) y
        (LRational x, LInteger  y) -> return $ Just $ x == (fromInteger y)
        (LRational x, LRational y) -> return $ Just $ x == y
        (LRational x, LDouble   y) -> return $ Just $ doRatDbl x y
        (LDouble   x, LInteger  y) -> return $ Just $ doRatDbl (fromInteger y) x
        (LDouble   x, LRational y) -> return $ Just $ doRatDbl y x
        (LDouble   x, LDouble   y) -> return $ Just $ x == y
        (LString x, LString y) -> return $ Just $ x == y
        _ -> do
            ka <- lxKey a
            kb <- lxKey b
            if ka == kb
                then return $ Just $ True
                else return $ Nothing
    where
    doRatDbl x y =
        if isNaN y || isInfinite y
            then (fromRational x) == y
            else x == (toRational y)


lxRawConcat
    :: LuaValue q s
    -> LuaValue q s
    -> LuaState q s (Maybe (LuaValue q s))
lxRawConcat a b = do
    case (a, b) of
        (LString x, LString y) -> return $ Just $ LString $ BSt.concat [x, y]
        _ -> return $ Nothing


lxRawLen
    :: LuaValue q s
    -> LuaState q s (Maybe Integer)
lxRawLen a = do
    case a of
        LString x -> return $ Just $ toInteger (BSt.length x)
        LTable _ table -> Just <$> lxTableLength table
        _ -> return $ Nothing


lxCall
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s [LuaValue q s]
lxCall a args = do
    case a of
        LFunction _ f -> f args
        _ -> do
            lxMetatable a (\x mt ->
                case lmtCall mt of
                    Just metaop -> metaop x args
                    Nothing -> lxError $ errWrongCall a)


lxAsString
    :: LuaValue q s
    -> LuaState q s BSt.ByteString
lxAsString a = do
    case a of
        LNil -> return $ BSt.pack $ show a
        LBool _ -> return $ BSt.pack $ show a
        LInteger _ -> return $ BSt.pack $ show a
        LRational _ -> return $ BSt.pack $ show a
        LDouble _ -> return $ BSt.pack $ show a
        LString s -> return $ s
        _ -> do
            lxMetatable a (\x mt ->
                case lmtToString mt of
                    Just metaop -> do
                        er <- lxTry $ metaop x
                        case er of
                            Left err -> do
                                lxWarn err
                                return $ BSt.pack $ show a
                            Right r -> return $ r
                    Nothing -> return $ BSt.pack $ show a)


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
                    key <- lxKey b
                    result <- lxGetTable key table
                    case result of
                        Just ret -> return $ ret
                        Nothing -> metaGet $ return LNil
        _ -> metaGet $ lxError $ errWrongTable a
    where
    metaGet def = do
        lxMetatable a (\x mt ->
            case lmtIndex mt of
                Just metaop -> metaop x b
                Nothing -> def)


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
                    key <- lxKey b
                    lxMetatable a (\x mt ->
                        case lmtNewIndex mt of
                            Just metaop -> do
                                LuaTableBody items mt <- lxRead table
                                case M.alterF alterBody key items of
                                    Nothing -> metaop x b c
                                    Just items' -> do
                                        lxWrite table $ LuaTableBody items' mt
                            Nothing -> lxSetTable key c table)
        _ -> metaSet $ lxError $ errWrongTable a
    where
    alterBody (Just oldv) =
        case c of
            LNil -> Just $ Nothing
            _ -> Just $ Just c
    alterBody (Nothing) = Nothing
    metaSet def = do
        lxMetatable a (\x mt ->
            case lmtNewIndex mt of
                Just metaop -> metaop x b c
                Nothing -> def)


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
