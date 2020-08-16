{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Lua.Coroutine (
    CoroutineT,
    Thread(..),
    lift,
    raise,
    try,
    wrap,
    yield,
) where


import Control.Monad


newtype CoroutineT a b e f t = CoroutineT (forall r .
       (forall u . f u -> (u -> r) -> r)
    -> (b -> (a -> r) -> r)
    -> (e -> r)
    -> (t -> r)
    -> r)


raise :: e -> CoroutineT a b e f t
raise x = CoroutineT $ \_ _ onRaise _ -> do
    onRaise x


yield :: b -> CoroutineT a b e f a
yield outv = CoroutineT $ \_ onYield _ onResult -> do
    onYield outv onResult


try :: CoroutineT a b e f t -> CoroutineT a b e f (Either e t)
try (CoroutineT body) = CoroutineT $ \onLift onYield _ onResult -> do
    body onLift onYield (onResult . Left) (onResult . Right)


lift :: f t -> CoroutineT a b e f t
lift act = CoroutineT $ \onLift _ _ onResult -> do
    onLift act onResult


instance Functor (CoroutineT a b e f) where
    fmap f (CoroutineT body) = do
        CoroutineT $ \onLift onYield onRaise onResult -> do
            body onLift onYield onRaise (onResult . f)


instance Applicative (CoroutineT a b e f) where
    pure x = CoroutineT $ \_ _ _ onResult -> do
        onResult x
    (<*>) = ap


instance Monad (CoroutineT a b e f) where
    return = pure
    CoroutineT body1 >>= after = do
        CoroutineT $ \onLift onYield onRaise onResult -> do
            body1 onLift onYield onRaise $ \x ->
                case x `seq` after x of
                    CoroutineT body2 -> body2 onLift onYield onRaise onResult


data Thread a b e f t where
    Pure :: !t -> Thread a b e f t
    Error :: !e -> Thread a b e f t
    Yield :: !b -> (a -> Thread a b e f t) -> Thread a b e f t
    Lift :: !(f u) -> (u -> Thread a b e f t) -> Thread a b e f t


wrap
    :: CoroutineT a b e f t
    -> Thread a b e f t
wrap (CoroutineT body) = do
    body
        Lift
        Yield
        Error
        Pure
