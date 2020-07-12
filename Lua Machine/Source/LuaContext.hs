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


module LuaContext (
) where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Bits
import Data.Maybe
import Data.String
import Data.STRef
import Type.Reflection
import qualified Data.Ratio as R
import qualified Data.Map as M
import qualified Data.ByteString as BSt
import qualified Coroutine as Y


infixr 9 ./
(./) :: (t -> u) -> (a -> b -> t) -> a -> b -> u
f ./ g = \a b -> f (g a b)


class (Monad m) => LuaHost s m | m -> s where
    liftST :: ST s a -> m a
    tryLiftIO :: IO a -> m (Maybe a)


instance LuaHost s (ST s) where
    liftST act = act
    tryLiftIO act = return $ Nothing


instance LuaHost RealWorld IO where
    liftST act = stToIO act
    tryLiftIO act = fmap Just act


type LuaMetaopUnary r s t =
    forall m . (LuaHost s m)
        => Maybe (t s -> LuaContextT s m r)

type LuaMetaopBinary r s t =
    forall m u . (LuaHost s m, Typeable u)
        => Maybe (
            t s -> u s -> LuaContextT s m r,
            u s -> t s -> LuaContextT s m r)

data LuaMetatable s (t :: * -> *) = LuaMetatable {
    lmtTypify :: forall a . ((Typeable t) => t s -> a) -> t s -> a,
    lmtMetatable :: LuaValue s,
    lmtKey :: LuaKey s,
    lmtTypename :: (forall a . (IsString a) => Maybe a),
    lmtAdd :: LuaMetaopBinary (LuaValue s) s t,
    lmtSub :: LuaMetaopBinary (LuaValue s) s t,
    lmtMul :: LuaMetaopBinary (LuaValue s) s t,
    lmtDiv :: LuaMetaopBinary (LuaValue s) s t,
    lmtMod :: LuaMetaopBinary (LuaValue s) s t,
    lmtPow :: LuaMetaopBinary (LuaValue s) s t,
    lmtUnm :: LuaMetaopUnary (LuaValue s) s t,
    lmtIDiv :: LuaMetaopBinary (LuaValue s) s t,
    lmtBAnd :: LuaMetaopBinary (LuaValue s) s t,
    lmtBOr :: LuaMetaopBinary (LuaValue s) s t,
    lmtBXor :: LuaMetaopBinary (LuaValue s) s t,
    lmtBNot :: LuaMetaopUnary (LuaValue s) s t,
    lmtShl :: LuaMetaopBinary (LuaValue s) s t,
    lmtShr :: LuaMetaopBinary (LuaValue s) s t,
    lmtConcat :: LuaMetaopBinary (LuaValue s) s t,
    lmtLen :: LuaMetaopUnary (LuaValue s) s t,
    lmtEq :: LuaMetaopBinary Bool s t,
    lmtLt :: LuaMetaopBinary Bool s t,
    lmtLe :: LuaMetaopBinary Bool s t,
    lmtIndex :: (forall m . (LuaHost s m)
        => Maybe (t s
        -> LuaValue s
        -> LuaContextT s m (LuaValue s))),
    lmtNewIndex :: (forall m . (LuaHost s m)
        => Maybe (t s
        -> LuaValue s
        -> LuaValue s
        -> LuaContextT s m ())),
    lmtCall :: (forall m . (LuaHost s m)
        => Maybe (t s
        -> [LuaValue s]
        -> LuaContextT s m [LuaValue s]))}


lvalEmptyMetatable :: (Typeable t) => LuaMetatable s t
lvalEmptyMetatable
    = LuaMetatable
        ($)
        LNil LKNil
        Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing


type LuaTable s = STRef s (M.Map (LuaKey s) (LuaValue s))


type LuaFunction s =
    forall m . (LuaHost s m)
        => [LuaValue s]
        -> LuaContextT s m [LuaValue s]


-- type LuaThread s =
    -- forall m . (LuaHost s m)
        -- => STRef s ([LuaValue s] -> LuaContextT s m [LuaValue s])


data LuaValue s where
    LNil :: LuaValue s
    LBool :: Bool -> LuaValue s
    LInteger :: Integer -> LuaValue s
    LRational :: Rational -> LuaValue s
    LDouble :: Double -> LuaValue s
    LString :: BSt.ByteString -> LuaValue s
    LFunction
        :: Int
        -> LuaFunction s
        -> LuaValue s
    LThread
        :: Int
        -- -> LuaThread s
        -> ()
        -> LuaValue s
    LTable
        :: Int
        -> LuaTable s
        -> LuaMetatable s LuaValue
        -> LuaValue s
    LUserdata
        :: Int
        -> t s
        -> LuaMetatable s t
        -> LuaValue s


instance Show (LuaValue s) where
    show LNil = "nil"
    show (LBool False) = "false"
    show (LBool True) = "true"
    show (LInteger i) = show i
    show (LRational r) = show (R.numerator r) ++ "/" ++ show (R.denominator r)
    show (LDouble d) = show d
    show (LString s) = show s
    show (LFunction i _) = "<function " ++ " #" ++ show i ++ ">"
    show (LThread i _) = "<thread " ++ " #" ++ show i ++ ">"
    show (LTable i _ mt)
        = "<" ++ fromMaybe "table " (lmtTypename mt)
        ++ " #" ++ show i ++ ">"
    show (LUserdata i _ mt)
        = "<" ++ fromMaybe "userdata " (lmtTypename mt)
        ++ " #" ++ show i ++ ">"


lvalTypename :: (IsString a) => LuaValue s -> a
lvalTypename LNil = "nil"
lvalTypename (LBool False) = "boolean"
lvalTypename (LBool True) = "boolean"
lvalTypename (LInteger i) = "number"
lvalTypename (LRational r) = "number"
lvalTypename (LDouble d) = "number"
lvalTypename (LString s) = "string"
lvalTypename (LFunction i _) = "function"
lvalTypename (LThread i _) = "thread"
lvalTypename (LTable i _ mt) = fromMaybe "table" (lmtTypename mt)
lvalTypename (LUserdata i _ mt) = fromMaybe "userdata" (lmtTypename mt)


data LuaKey s
    = LKNil
    | LKBool Bool
    | LKRational Rational
    | LKString BSt.ByteString
    | LKObject Int
    deriving (Show, Ord, Eq)


lvalKey :: LuaValue s -> LuaKey s
lvalKey LNil = LKNil
lvalKey (LBool b) = LKBool b
lvalKey (LInteger i) = LKRational $ fromInteger i
lvalKey (LRational r) = LKRational $ r
lvalKey (LDouble d) = LKRational $ toRational d
lvalKey (LString str) = LKString $ str
lvalKey (LFunction i _) = LKObject $ i
lvalKey (LThread i _) = LKObject $ i
lvalKey (LTable i _ _ ) = LKObject $ i
lvalKey (LUserdata i _ _) = LKObject $ i


errDivideZero :: LuaValue s
errDivideZero = LString $ "Attempt to divide by zero"

errNilIndex :: LuaValue s
errNilIndex = LString $ "Attempt to index a table with a nil"

errWrongArith1 :: BSt.ByteString -> LuaValue s
errWrongArith1 typename
    = LString $ BSt.concat [
        "Attempt to perform arithmetic on a ", typename]

errWrongArith2 :: BSt.ByteString -> BSt.ByteString -> LuaValue s
errWrongArith2 tn1 tn2
    = LString $ BSt.concat [
        "Attempt to perform arithmetic between a ", tn1, " and a ", tn2]

errWrongBit :: LuaValue s
errWrongBit = LString $
    "Attempt to perform bitwise on a non-integer number"

errWrongConcat :: BSt.ByteString -> LuaValue s
errWrongConcat typename
    = LString $ BSt.concat [
        "Attempt to concatenate a ", typename]

errWrongTable :: BSt.ByteString -> LuaValue s
errWrongTable typename
    = LString $ BSt.concat ["Attempt to index a ", typename]

errWrongYield :: LuaValue s
errWrongYield = LString "Attempt to yield in a non-yieldable context"


data LuaState s
    = LuaState
        (
            STRef s Int,
            STRef s (LuaMetatable s LuaValue),
            STRef s (LuaMetatable s LuaValue),
            STRef s (LuaMetatable s LuaValue),
            STRef s (LuaMetatable s LuaValue),
            STRef s (LuaMetatable s LuaValue),
            STRef s (LuaMetatable s LuaValue))
        [String]


lsCounter (LuaState (cnt, mt0, mt1, mt2, mt3, mt4, mt5) tb) = cnt
lsNilMetatable (LuaState (cnt, mt0, mt1, mt2, mt3, mt4, mt5) tb) = mt0
lsBoolMetatable (LuaState (cnt, mt0, mt1, mt2, mt3, mt4, mt5) tb) = mt1
lsNumberMetatable (LuaState (cnt, mt0, mt1, mt2, mt3, mt4, mt5) tb) = mt2
lsStringMetatable (LuaState (cnt, mt0, mt1, mt2, mt3, mt4, mt5) tb) = mt3
lsFunctionMetatable (LuaState (cnt, mt0, mt1, mt2, mt3, mt4, mt5) tb) = mt4
lsThreadMetatable (LuaState (cnt, mt0, mt1, mt2, mt3, mt4, mt5) tb) = mt5
lsTraceback (LuaState vars tb) = tb

lsTracebackPush :: String -> LuaState s -> LuaState s
lsTracebackPush line (LuaState vars tb)
    = LuaState vars (line:tb)


lcxNewState :: ST s (LuaState s)
lcxNewState = LuaState
    <$> ((,,,,,,)
        <$> newSTRef 0
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable
        <*> newSTRef lvalEmptyMetatable)
    <*> return []


newtype LuaContextT s m t = LuaContextT {
    runLuaContextT
        :: ReaderT
            (LuaState s)
            (Y.CoroutineT [LuaValue s] [LuaValue s] (LuaValue s) m)
            t}


lcxRun
    :: (Monad m)
    => LuaContextT s m t
    -> LuaState s
    -> m (Either (LuaValue s) t)
lcxRun (LuaContextT act) state = drive (runReaderT act state)
    where
    drive (Y.Pure x) = return $ Right x
    drive (Y.Error e) = return $ Left e
    drive (Y.Hold _ g) = drive $ g $ Left errWrongYield
    drive (Y.Lift d) = d >>= drive


instance (LuaHost s m) => LuaHost s (LuaContextT s m) where
    liftST act = LuaContextT $ lift $ lift $ liftST act
    tryLiftIO act = LuaContextT $ lift $ lift $ tryLiftIO act


instance MonadTrans (LuaContextT s) where
    lift act = LuaContextT $ lift $ lift $ act


instance (Monad m) => MonadReader (LuaState s) (LuaContextT s m) where
    ask = LuaContextT $ ask
    local f act = LuaContextT $ local f $ runLuaContextT act
    reader f = LuaContextT $ reader f


instance (Monad m) => Monad (LuaContextT s m) where
    return x = LuaContextT $ return x
    LuaContextT st >>= f = LuaContextT $ st >>= (runLuaContextT . f)


instance (Monad m) => Functor (LuaContextT s m) where
    fmap f (LuaContextT st) = LuaContextT $ fmap f st


instance (Monad m) => Applicative (LuaContextT s m) where
    pure x = LuaContextT $ pure x
    LuaContextT sta <*> LuaContextT stb = LuaContextT $ sta <*> stb


lcxIncrementCounter :: (LuaHost s m) => LuaContextT s m Int
lcxIncrementCounter = do
    state <- ask
    let ref = lsCounter state
    lcxModify ref (+1)
    lcxRead ref


lcxAlloc :: (LuaHost s m) => a -> LuaContextT s m (STRef s a)
lcxAlloc x = liftST $ newSTRef x


lcxRead :: (LuaHost s m) => STRef s a -> LuaContextT s m a
lcxRead ref = liftST $ readSTRef ref


lcxWrite :: (LuaHost s m) => STRef s a -> a -> LuaContextT s m ()
lcxWrite ref x = liftST $ writeSTRef ref x


lcxModify :: (LuaHost s m) => STRef s a -> (a -> a) -> LuaContextT s m ()
lcxModify ref f = liftST $ modifySTRef' ref f


lcxAllocKey :: (LuaHost s m) => LuaContextT s m (LuaKey s)
lcxAllocKey = LKObject <$> lcxIncrementCounter


lcxAllocTable
    :: (LuaHost s m)
    => [(LuaKey s, LuaValue s)]
    -> LuaContextT s m (Int, LuaTable s)
lcxAllocTable elems
    = (,) <$> lcxIncrementCounter <*> (lcxAlloc $ M.fromList elems)


lcxGetTable
    :: (LuaHost s m)
    => LuaKey s
    -> LuaTable s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxGetTable key table = M.lookup key <$> lcxRead table


lcxSetTable
    :: (LuaHost s m)
    => LuaKey s
    -> LuaValue s
    -> LuaTable s
    -> LuaContextT s m ()
lcxSetTable key value table
    = lcxWrite table =<< M.insert key value <$> lcxRead table


lcxMetatable
    :: (LuaHost s m)
    => LuaValue s
    -> (forall t . (Typeable t)
        => t s
        -> LuaMetatable s t
        -> LuaContextT s m a)
    -> LuaContextT s m a
lcxMetatable x f = do
    case x of
        LNil -> f x =<< (lcxRead =<< lsNilMetatable <$> ask)
        LBool _ -> f x =<< (lcxRead =<< lsBoolMetatable <$> ask)
        LInteger _ -> f x =<< (lcxRead =<< lsNumberMetatable <$> ask)
        LRational _ -> f x =<< (lcxRead =<< lsNumberMetatable <$> ask)
        LDouble _ -> f x =<< (lcxRead =<< lsNumberMetatable <$> ask)
        LString _ -> f x =<< (lcxRead =<< lsStringMetatable <$> ask)
        LFunction _ _ -> f x =<< (lcxRead =<< lsFunctionMetatable <$> ask)
        LThread _ _ -> f x =<< (lcxRead =<< lsThreadMetatable <$> ask)
        LTable _ _ mt -> f x mt
        LUserdata _ ud mt -> lmtTypify mt f ud mt


lcxPerformMetaopUnary
    :: (LuaHost s m)
    => (forall t . LuaMetatable s t -> LuaMetaopUnary r s t)
    -> LuaValue s
    -> LuaContextT s m (Maybe r)
lcxPerformMetaopUnary metaf a = do
    lcxMetatable a (\x mt ->
        case metaf mt of
            Just metaop -> Just <$> metaop x
            Nothing -> return $ Nothing)


lcxPerformMetaopBinary
    :: (LuaHost s m)
    => (forall t . LuaMetatable s t -> LuaMetaopBinary r s t)
    -> LuaValue s
    -> LuaValue s
    -> LuaContextT s m (Maybe r)
lcxPerformMetaopBinary metaf a b = do
    lcxMetatable a (\x mta ->
        lcxMetatable b (\y mtb -> do
            case metaf mta of
                Just (ma, _) -> Just <$> ma x y
                Nothing ->
                    case metaf mtb of
                        Just (_, mb) -> Just <$> mb x y
                        Nothing -> return $ Nothing))


lcxYield :: (LuaHost s m) => [LuaValue s] -> LuaContextT s m [LuaValue s]
lcxYield vals = LuaContextT $ lift $ Y.yield vals


lcxError :: (LuaHost s m) => (LuaValue s) -> LuaContextT s m a
lcxError err = LuaContextT $ lift $ Y.raise err


lcxTry
    :: (LuaHost s m)
    => LuaContextT s m t
    -> LuaContextT s m (Either (LuaValue s) t)
lcxTry (LuaContextT act) = LuaContextT $ mapReaderT Y.try $ act


lcxTracebackWithin
    :: (LuaHost s m)
    => String
    -> LuaContextT s m t
    -> LuaContextT s m t
lcxTracebackWithin line act = local (lsTracebackPush line) act


lcxPerformUnary
    :: (LuaHost s m)
    => (LuaValue s -> LuaContextT s m (Maybe (LuaValue s)))
    -> (forall t . LuaMetatable s t -> LuaMetaopUnary (LuaValue s) s t)
    -> LuaContextT s m (LuaValue s)
    -> LuaValue s
    -> LuaContextT s m (LuaValue s)
lcxPerformUnary raw metaf def a = do
    rr <- raw a
    case rr of
        Just ret -> return $ ret
        Nothing -> do
            mr <- lcxPerformMetaopUnary metaf a
            case mr of
                Just ret' -> return $ ret'
                Nothing -> def


lcxPerformBinary
    :: (LuaHost s m)
    => (LuaValue s -> LuaValue s -> LuaContextT s m (Maybe (LuaValue s)))
    -> (forall t . LuaMetatable s t -> LuaMetaopBinary (LuaValue s) s t)
    -> LuaContextT s m (LuaValue s)
    -> LuaValue s
    -> LuaValue s
    -> LuaContextT s m (LuaValue s)
lcxPerformBinary raw metaf def a b = do
    rr <- raw a b
    case rr of
        Just ret -> return $ ret
        Nothing -> do
            mr <- lcxPerformMetaopBinary metaf a b
            case mr of
                Just ret' -> return $ ret'
                Nothing -> def


lcxRawArithUnm
    :: (LuaHost s m)
    => LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithUnm a = do
    case a of
        LInteger x  -> return $ Just $ LInteger  $ negate x
        LRational x -> return $ Just $ LRational $ negate x
        LDouble x   -> return $ Just $ LDouble   $ negate x
        _ -> return $ Nothing


lcxRawArithBNot
    :: (LuaHost s m)
    => LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithBNot a = do
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
                then lcxError $ errWrongBit
                else return $ R.numerator d
    cvDbl x =
        if isNaN x || isInfinite x
            then lcxError $ errWrongBit
            else cvRat (toRational x)


lcxRawArithNum
    :: (LuaHost s m)
    => (forall t . (Num t) => t -> t -> t)
    -> LuaValue s
    -> LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithNum op a b = do
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


lcxRawArithDiv
    :: (LuaHost s m)
    => LuaValue s
    -> LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithDiv a b = do
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


lcxRawArithIDiv
    :: (LuaHost s m)
    => LuaValue s
    -> LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithIDiv a b = do
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
            then lcxError $ errDivideZero
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


lcxRawArithMod
    :: (LuaHost s m)
    => LuaValue s
    -> LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithMod a b = do
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
            then lcxError $ errDivideZero
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


lcxRawArithPow
    :: (LuaHost s m)
    => LuaValue s
    -> LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithPow a b = do
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


lcxRawArithBitwise
    :: (LuaHost s m)
    => (Integer -> Integer -> Integer)
    -> LuaValue s
    -> LuaValue s
    -> LuaContextT s m (Maybe (LuaValue s))
lcxRawArithBitwise op a b = do
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
                then lcxError $ errWrongBit
                else return $ R.numerator d
    cvDbl x =
        if isNaN x || isInfinite x
            then lcxError $ errWrongBit
            else cvRat (toRational x)


luaArithUnm a = lcxPerformUnary
    lcxRawArithUnm
    lmtUnm
    (lcxError $ errWrongArith1 (lvalTypename a))
    a

luaArithBNot a = lcxPerformUnary
    lcxRawArithBNot
    lmtBNot
    (lcxError $ errWrongArith1 (lvalTypename a))
    a

luaArithAdd a b = lcxPerformBinary
    (lcxRawArithNum (+))
    lmtAdd
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithSub a b = lcxPerformBinary
    (lcxRawArithNum (-))
    lmtSub
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithMul a b = lcxPerformBinary
    (lcxRawArithNum (-))
    lmtMul
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithDiv a b = lcxPerformBinary
    lcxRawArithDiv
    lmtDiv
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithIDiv a b = lcxPerformBinary
    lcxRawArithIDiv
    lmtIDiv
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithMod a b = lcxPerformBinary
    lcxRawArithMod
    lmtMod
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithPow a b = lcxPerformBinary
    lcxRawArithPow
    lmtPow
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithBAnd a b = lcxPerformBinary
    (lcxRawArithBitwise (.&.))
    lmtBAnd
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithBOr a b = lcxPerformBinary
    (lcxRawArithBitwise (.|.))
    lmtBOr
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithBXor a b = lcxPerformBinary
    (lcxRawArithBitwise xor)
    lmtBXor
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithShl a b = lcxPerformBinary
    (lcxRawArithBitwise (\x y -> shiftL x (fromInteger y)))
    lmtShl
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b

luaArithShr a b = lcxPerformBinary
    (lcxRawArithBitwise (\x y -> shiftR x (fromInteger y)))
    lmtShr
    (lcxError $ errWrongArith2 (lvalTypename a) (lvalTypename b))
    a b


luaCreateFunction
    :: (LuaHost s m)
    => (forall m' . (LuaHost s m')
        => [LuaValue s]
        -> LuaContextT s m' [LuaValue s])
    -> LuaContextT s m (LuaValue s)
luaCreateFunction fn = do
    cnt <- lcxIncrementCounter
    return $ LFunction cnt fn


luaCreateTable
    :: (LuaHost s m)
    => [(LuaValue s, LuaValue s)]
    -> LuaContextT s m (LuaValue s)
luaCreateTable init = do
    (cnt, table) <- lcxAllocTable $
        map
            (\(kv, value) -> (lvalKey kv, value))
            init
    return $ LTable cnt table lvalEmptyMetatable


luaError :: (LuaHost s m) => LuaValue s -> LuaContextT s m a
luaError e = lcxError e


luaGetMetatable
    :: (LuaHost s m)
    => LuaValue s
    -> LuaContextT s m (LuaValue s)
luaGetMetatable =
    undefined


luaNewTable
    :: (LuaHost s m)
    => LuaContextT s m (LuaValue s)
luaNewTable = luaCreateTable []


luaNewThread
    :: (LuaHost s m)
    => LuaContextT s m (LuaValue s)
luaNewThread = do
    undefined


class LuaPush a where
    luaPush :: a -> LuaValue s

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


luaRawConcat
    :: LuaValue s
    -> LuaValue s
    -> Maybe (LuaValue s)
luaRawConcat a b = do
    case (a, b) of
        (LString x, LString y) -> Just $ luaPush $ BSt.concat [x, y]
        _ -> Nothing


luaRawGet
    :: (LuaHost s m)
    => LuaValue s
    -> LuaValue s
    -> LuaContextT s m (LuaValue s)
luaRawGet kv tv = do
    case tv of
        LTable _ table _ -> do
            case kv of
                LNil -> return $ LNil
                _ -> do
                    let key = lvalKey kv
                    result <- lcxGetTable key table
                    case result of
                        Just ret -> return $ ret
                        Nothing -> return $ LNil
        _ -> lcxError $ errWrongTable $ lvalTypename tv


luaRawSet
    :: (LuaHost s m)
    => LuaValue s
    -> LuaValue s
    -> LuaValue s
    -> LuaContextT s m ()
luaRawSet kv value tv = do
    case tv of
        LTable _ table _ -> do
            case kv of
                LNil -> lcxError $ errNilIndex
                _ -> do
                    let key = lvalKey kv
                    lcxSetTable key value table
        _ -> lcxError $ errWrongTable $ lvalTypename tv


-- testPrintTraceback :: (LuaHost s m) => String -> LuaContextT s m ()
-- testPrintTraceback header = do
    -- tb <- lsTraceback <$> ask
    -- tryLiftIO (do
        -- putStrLn $ header
        -- forM_ tb (\line ->
            -- putStrLn $ line))
    -- return ()


-- testfunc :: (LuaHost s m) => LuaContextT s m (LuaValue s)
-- testfunc = do
    -- lcxTracebackWithin "foo" (do
        -- testPrintTraceback "A"
        -- let lv = luaPush ("aaa" :: BSt.ByteString)
        -- lcxTry (do
            -- lcxTracebackWithin "bar" (do
                -- tryLiftIO $ print lv
                -- testPrintTraceback "B"
                -- lcxYield []
                -- testPrintTraceback "C"))
        -- testPrintTraceback "D"
        -- let a = LInteger 10
        -- let b = LString "asdf"
        -- c <- luaArithAdd a b
        -- return $ c)


-- test' :: IO ()
-- test' = do
    -- let act = runST (do
        -- state <- lcxNewState
        -- r <- lcxRun testfunc state
        -- case r of
            -- Left e -> return $ print $ e
            -- Right i -> return $ print $ i)
    -- act


-- test :: IO ()
-- test = do
    -- state <- stToIO lcxNewState
    -- r <- lcxRun testfunc state
    -- case r of
        -- Left e -> print $ e
        -- Right i -> print $ i
