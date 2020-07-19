{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}


module LuaState (
) where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Bits
import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.STRef
import Type.Reflection
import SourceRange (SourceRange)
import qualified Data.Ratio as R
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BSt
import qualified Coroutine as Y


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
        :: Maybe (t q s
        -> LuaValue q s
        -> LuaState q s (LuaValue q s)),
    lmtNewIndex
        :: Maybe (t q s
        -> LuaValue q s
        -> LuaValue q s
        -> LuaState q s ()),
    lmtCall
        :: Maybe (t q s
        -> [LuaValue q s]
        -> LuaState q s [LuaValue q s]),
    lmtClose
        :: Maybe (t q s
        -> LuaState q s ())}


lvalEmptyMetatable :: (Typeable t) => LuaMetatable q s t
lvalEmptyMetatable
    = LuaMetatable
        ($)
        LNil
        Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing


newtype LuaId = LuaId Int deriving (Show, Eq, Ord)


data LuaTableBody q s = LuaTableBody {
    ltbItems :: M.Map (LuaKey q s) (LuaValue q s),
    ltbMetatable :: LuaMetatable q s LuaValue}


type LuaTable q s = STRef s (LuaTableBody q s)


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


type LuaThread q s = STRef s (LuaThreadState q s)


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
    show (LRational r) = show (R.numerator r) ++ "/" ++ show (R.denominator r)
    show (LDouble d) = show d
    show (LString s) = show s
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


errDivideZero :: LuaValue q s
errDivideZero = LString $ "Attempt to divide by zero"

errNilIndex :: LuaValue q s
errNilIndex = LString $ "Attempt to index a table with a nil"

errNonSuspended :: LuaValue q s
errNonSuspended = LString $ "Attempt to resume a non-suspended coroutine"

errWrongArith1 :: BSt.ByteString -> LuaValue q s
errWrongArith1 typename
    = LString $ BSt.concat [
        "Attempt to perform arithmetic on a ", typename]

errWrongArith2 :: BSt.ByteString -> BSt.ByteString -> LuaValue q s
errWrongArith2 tn1 tn2
    = LString $ BSt.concat [
        "Attempt to perform arithmetic between a ", tn1, " and a ", tn2]

errWrongBit :: LuaValue q s
errWrongBit = LString $
    "Attempt to perform bitwise on a non-integer number"

errWrongCall :: BSt.ByteString -> LuaValue q s
errWrongCall typename = LString $ BSt.concat [
    "Attempt to call a ", typename, "number"]

errWrongCompare :: BSt.ByteString -> BSt.ByteString -> LuaValue q s
errWrongCompare tn1 tn2
    = LString $ BSt.concat [
        "Attempt to compare a ", tn1, " and a ", tn2]

errWrongConcat :: BSt.ByteString -> BSt.ByteString -> LuaValue q s
errWrongConcat tn1 tn2
    = LString $ BSt.concat [
        "Attempt to concatenate a ", tn1, " and a ", tn2]

errWrongIO :: LuaValue q s
errWrongIO = LString $
    "Attempt to perform IO where it's unsupported"

errWrongLen :: BSt.ByteString -> LuaValue q s
errWrongLen typename
    = LString $ BSt.concat [
        "Attempt to take length of a ", typename]

errWrongResume :: BSt.ByteString -> LuaValue q s
errWrongResume typename
    = LString $ BSt.concat [
        "Attempt to resume a ", typename]

errWrongMetatableOwner :: BSt.ByteString -> LuaValue q s
errWrongMetatableOwner typename
    = LString $ BSt.concat [
        "Attempt to set a metatable for a ", typename]

errWrongMetatableValue :: LuaValue q s
errWrongMetatableValue = LString $
    "Attempt to set a non-table value as a metatable"

errWrongTable :: BSt.ByteString -> LuaValue q s
errWrongTable typename
    = LString $ BSt.concat ["Attempt to index a ", typename]

errWrongThreadFunc :: BSt.ByteString -> LuaValue q s
errWrongThreadFunc typename
    = LString $ BSt.concat ["Attempt to create a coroutine from ", typename]

errWrongYield :: LuaValue q s
errWrongYield = LString "Attempt to yield in a non-yieldable context"


data LuaEnvironment q s = LuaEnvironment {
    lenvCounter :: STRef s Int,
    lenvNilMetatable :: STRef s (LuaMetatable q s LuaValue),
    lenvBoolMetatable :: STRef s (LuaMetatable q s LuaValue),
    lenvNumberMetatable :: STRef s (LuaMetatable q s LuaValue),
    lenvStringMetatable :: STRef s (LuaMetatable q s LuaValue),
    lenvFunctionMetatable :: STRef s (LuaMetatable q s LuaValue),
    lenvThreadMetatable :: STRef s (LuaMetatable q s LuaValue),
    lenvWarnHandler :: STRef s (LuaValue q s -> LuaState q s ())}


type LuaErrorHandler q s = LuaValue q s -> LuaState q s (LuaValue q s)


data LuaStackFrame q s = LuaStackFrame {
    lsfFunction :: Maybe (LuaFunction q s),
    lsfDefLocation :: Maybe SourceRange,
    lsfCurrentLocation :: Maybe SourceRange,
    lsfUpvalues :: [STRef s (LuaValue q s)],
    lsfLocals :: [STRef s (LuaValue q s)]}


lsfModifyCurrentLocation
    :: (Maybe SourceRange -> Maybe SourceRange)
    -> LuaStackFrame q s
    -> LuaStackFrame q s
lsfModifyCurrentLocation f (LuaStackFrame b d c u l)
    = LuaStackFrame b d (f c) u l


lsfModifyLocals
    :: ([STRef s (LuaValue q s)] -> [STRef s (LuaValue q s)])
    -> LuaStackFrame q s
    -> LuaStackFrame q s
lsfModifyLocals f (LuaStackFrame b d c u l)
    = LuaStackFrame b d c u (f l)


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


lctxModifyStackTop
    :: (LuaStackFrame q s -> LuaStackFrame q s)
    -> LuaContext q s
    -> LuaContext q s
lctxModifyStackTop f = lctxModifyStack sf
    where
    sf (t:ts) = (f t):ts
    sf [] = []


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


lxRunST
    :: (forall q . LuaState q s t)
    -> ST s (Either String t)
lxRunST = lxRunT resolveST resolveIO resolveYield onError onPure
    where
    resolveST act = act
    resolveIO act = return $ Nothing
    resolveYield vals = return $ Left $ errWrongYield
    onError (LString msg)
        = return $ Left $ ((toEnum . fromEnum) <$> BSt.unpack msg)
    onError _ = return $ Left $ "Unknown error"
    onPure x = return $ Right $ x


lxRunIO
    :: (forall q . LuaState q RealWorld t)
    -> IO (Either String t)
lxRunIO = lxRunT resolveST resolveIO resolveYield onError onPure
    where
    resolveST act = stToIO $ act
    resolveIO act = Just <$> act
    resolveYield vals = return $ Left $ errWrongYield
    onError (LString msg)
        = return $ Left $ ((toEnum . fromEnum) <$> BSt.unpack msg)
    onError _ = return $ Left $ "Unknown error"
    onPure x = return $ Right $ x


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
                    let msgStr = (toEnum . fromEnum) <$> BSt.unpack msg
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


lxAlloc :: a -> LuaState q s (STRef s a)
lxAlloc x = lxLiftST $ newSTRef x


lxRead :: STRef s a -> LuaState q s a
lxRead ref = lxLiftST $ readSTRef ref


lxWrite :: STRef s a -> a -> LuaState q s ()
lxWrite ref x = lxLiftST $ writeSTRef ref x


lxModify :: STRef s a -> (a -> a) -> LuaState q s ()
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
    if R.denominator r == 1
        then return $ LKInteger $ R.numerator r
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


lxCar :: [LuaValue q s] -> LuaValue q s
lxCar [] = LNil
lxCar (a:rest) = a


lxCdr :: [LuaValue q s] -> [LuaValue q s]
lxCdr [] = []
lxCdr (a:rest) = rest


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
        lmtClose = mkMetaopClose <$!> M.lookup (LKString "__close") items}
    return $ metatable
    where
    mkMetaopUnary f = \a -> lxCar <$> luaCall f [a]
    mkMetaopBinary f = (
        \a b -> lxCar <$> luaCall f [a, b],
        \a b -> lxCar <$> luaCall f [a, b])
    mkMetaopBinaryBool f = (
        \a b -> listToBool <$> luaCall f [a, b],
        \a b -> listToBool <$> luaCall f [a, b])
    listToBool (LNil:_) = False
    listToBool (LBool False:_) = False
    listToBool (_:_) = True
    listToBool _ = False
    mkMetaopIndex (LFunction _ ff) = \a b -> lxCar <$> ff [a, b]
    mkMetaopIndex f = \a b -> luaGet f b
    mkMetaopNewIndex (LFunction _ ff) = \a b c -> () <$ ff [a, b, c]
    mkMetaopNewIndex f = \a b c -> luaSet f b c
    mkMetaopCall f = \a args -> luaCall f (a:args)
    mkMetaopClose f = \a -> () <$ luaCall f [a]
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
            if R.denominator d /= 1
                then lxError $ errWrongBit
                else return $ R.numerator d
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
            if R.denominator d /= 1
                then lxError $ errWrongBit
                else return $ R.numerator d
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


luaArithUnm a = lxPerformUnary
    lxRawArithUnm
    lmtUnm
    (lxError $ errWrongArith1 (lvalTypename a))
    a

luaArithBNot a = lxPerformUnary
    lxRawArithBNot
    lmtBNot
    (lxError $ errWrongArith1 (lvalTypename a))
    a

luaArithAdd a b = lxPerformBinary
    (lxRawArithNum (+))
    lmtAdd
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithSub a b = lxPerformBinary
    (lxRawArithNum (-))
    lmtSub
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithMul a b = lxPerformBinary
    (lxRawArithNum (-))
    lmtMul
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithDiv a b = lxPerformBinary
    lxRawArithDiv
    lmtDiv
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithIDiv a b = lxPerformBinary
    lxRawArithIDiv
    lmtIDiv
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithMod a b = lxPerformBinary
    lxRawArithMod
    lmtMod
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithPow a b = lxPerformBinary
    lxRawArithPow
    lmtPow
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithBAnd a b = lxPerformBinary
    (lxRawArithBitwise (.&.))
    lmtBAnd
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithBOr a b = lxPerformBinary
    (lxRawArithBitwise (.|.))
    lmtBOr
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithBXor a b = lxPerformBinary
    (lxRawArithBitwise xor)
    lmtBXor
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithShl a b = lxPerformBinary
    (lxRawArithBitwise (\x y -> shiftL x (fromInteger y)))
    lmtShl
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithShr a b = lxPerformBinary
    (lxRawArithBitwise (\x y -> shiftR x (fromInteger y)))
    lmtShr
    (lxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b


luaCall
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s [LuaValue q s]
luaCall a args = do
    case a of
        LFunction _ f -> f args
        _ -> do
            lxMetatable a (\x mt ->
                case lmtCall mt of
                    Just metaop -> metaop x args
                    Nothing -> lxError $ errWrongCall (lvalTypename a))


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
    (lxError $ errWrongCompare (lvalTypename a) (lvalTypename b))
    a b


luaCompareLe a b = lxPerformBinary
    (lxRawCompare (<=))
    lmtLe
    (lxError $ errWrongCompare (lvalTypename a) (lvalTypename b))
    a b


luaCompareEq a b = lxPerformBinary
    lxRawCompareEq
    lmtLe
    (return $ False)
    a b


luaConcat a b = lxPerformBinary
    lxRawConcat
    lmtConcat
    (lxError $ errWrongConcat (lvalTypename a) (lvalTypename b))
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
luaGet a b = do
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
        _ -> metaGet $ lxError $ errWrongTable (lvalTypename b)
    where
    metaGet def = do
        lxMetatable a (\x mt ->
            case lmtIndex mt of
                Just metaop -> metaop x b
                Nothing -> def)


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
                        _ -> lxError $ errWrongLen (lvalTypename a))


luaLiftIO
    :: IO t
    -> LuaState q s t
luaLiftIO act = do
    mr <- lxTryLiftIO act
    case mr of
        Nothing -> lxError $ errWrongIO
        Just r -> return $ r


luaNewTable
    :: LuaState q s (LuaValue q s)
luaNewTable = luaCreateTable []


luaNewThread
    :: LuaState q s (LuaValue q s)
luaNewThread = luaCreateThread (\init -> do
    case init of
        (LFunction _ f):args -> f args
        a:args -> lxError $ errWrongThreadFunc (lvalTypename a)
        _ -> lxError $ errWrongThreadFunc "nothing")


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
    luaPush x = LString $ BSt.pack ((toEnum . fromEnum) <$> x)


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
        _ -> lxError $ errWrongTable (lvalTypename a)


luaRawLen
    :: LuaValue q s
    -> LuaState q s Integer
luaRawLen a = do
    rr <- lxRawLen a
    case rr of
        Just len -> return $ len
        Nothing -> lxError $ errWrongLen (lvalTypename a)


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
        _ -> lxError $ errWrongTable (lvalTypename a)


luaResume
    :: LuaValue q s
    -> [LuaValue q s]
    -> LuaState q s (Either (LuaValue q s) [LuaValue q s])
luaResume a args = do
    case a of
        LThread _ thread -> lxResume thread (Right args)
        _ -> lxError $ errWrongResume (lvalTypename a)


luaSet
    :: LuaValue q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s ()
luaSet a b c = do
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
        _ -> metaSet $ lxError $ errWrongTable (lvalTypename a)
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
        _ -> lxError $ errWrongMetatableOwner (lvalTypename a)


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
    if R.denominator x == 1
        then Just $ R.numerator x
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
luaToString (LString b) = Just $ (toEnum . fromEnum) <$> BSt.unpack b
luaToString _ = Nothing


luaToThread
    :: LuaValue q s
    -> Maybe (LuaThread q s)
luaToThread (LThread _ thread) = Just $ thread
luaToThread _ = Nothing


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


testfunc :: LuaState q s String
testfunc = do
    let lv = luaPush ("aaa" :: BSt.ByteString)
    luaTry (do
        luaLiftIO $ print lv
        luaYield [])
    let a = LInteger 10
    let b = LString "asdf"
    c <- luaArithAdd a b
    return $ show c


test' :: IO ()
test' = do
    let r = runST $ lxRunST $ testfunc
    case r of
        Left e -> putStrLn $ e
        Right s -> putStrLn $ s


test :: IO ()
test = do
    r <- lxRunIO $ testfunc
    case r of
        Left e -> putStrLn $ e
        Right i -> putStrLn $ i
