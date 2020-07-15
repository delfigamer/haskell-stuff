{-# LANGUAGE RankNTypes #-}


module Coroutine (
    CoroutineT(..),
    raise,
    yield,
    try,
) where


import Control.Monad
import Control.Monad.Trans


data CoroutineT a b e m t
    = Pure t
    | Error e
    | Hold b (a -> CoroutineT a b e m t)
    | Lift (m (CoroutineT a b e m t))


raise :: e -> CoroutineT a b e m t
raise e = Error e


yield :: b -> CoroutineT a b e m a
yield y = Hold y (\x -> Pure x)


try :: (Monad m) => CoroutineT a b e m t -> CoroutineT a b e m (Either e t)
try (Pure x) = Pure (Right x)
try (Error e) = Pure (Left e)
try (Hold y g) = Hold y (try . g)
try (Lift d) = Lift (d >>= (return . try))


instance MonadTrans (CoroutineT a b e) where
    lift x = Lift (x >>= (return . Pure))


instance (Monad m) => Monad (CoroutineT a b e m) where
    return x = Pure x
    Pure x >>= sel = sel x
    Error e >>= sel = Error e
    Hold y g >>= sel = Hold y ((>>= sel) . g)
    Lift d >>= sel = Lift (d >>= (return . (>>= sel)))


instance (Monad m) => Functor (CoroutineT a b e m) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Error e) = Error e
    fmap f (Hold y g) = Hold y (fmap f . g)
    fmap f (Lift d) = Lift (fmap (fmap f) d)


instance (Monad m) => Applicative (CoroutineT a b e m) where
    pure = return
    x <*> y = x >>= (\f -> fmap f y)


instance (Monad m, Read e) => MonadFail (CoroutineT a b e m) where
    fail msg = Error (read msg)


instance (MonadIO m) => MonadIO (CoroutineT a b e m) where
    liftIO = lift . liftIO
