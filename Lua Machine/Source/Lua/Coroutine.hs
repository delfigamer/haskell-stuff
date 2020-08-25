{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Lua.Coroutine (
    Coroutine,
    Suspend,
    catch,
    execute,
    lift,
    raise,
    resume,
    suspend,
    yield,
) where


import Control.Monad


newtype Suspend a b e q = Suspend {
    runSuspend
        :: (b -> Suspend a b e q -> q)
        -> (e -> q)
        -> (b -> q)
        -> a
        -> q}


newtype Coroutine a b e q f t = Coroutine {
    runc
        :: (forall u . f u -> (u -> q) -> q)
        -> (b -> Suspend a b e q -> q)
        -> (e -> q)
        -> (b -> q)
        -> ((b -> Suspend a b e q -> q) -> (e -> q) -> (b -> q) -> e -> q)
        -> ((b -> Suspend a b e q -> q) -> (e -> q) -> (b -> q) -> t -> q)
        -> q}


instance Functor (Coroutine a b e q f) where
    fmap = liftM


instance Applicative (Coroutine a b e q f) where
    pure = return
    (<*>) = ap


instance Monad (Coroutine a b e q f) where
    return x = Coroutine $ \_ acYield acError acPure _ onPure -> do
        onPure acYield acError acPure x
    body1 >>= after = Coroutine $
        \resolve acYield1 acError1 acPure1 onError onPure -> do
            runc body1 resolve acYield1 acError1 acPure1 onError $
                \acYield2 acError2 acPure2 x -> do
                    runc (after x) resolve acYield2 acError2 acPure2
                        onError onPure


lift :: f t -> Coroutine a b e q f t
lift box = Coroutine $ \resolve acYield acError acPure _ onPure -> do
    resolve box $ onPure acYield acError acPure


yield :: b -> Coroutine a b e q f a
yield outv = Coroutine $ \_ acYield _ _ _ onPure -> do
    acYield outv $ Suspend onPure


raise :: e -> Coroutine a b e q f t
raise err = Coroutine $ \_ acYield acError acPure onError _ -> do
    onError acYield acError acPure err


catch
    :: Coroutine a b e q f t
    -> (e -> Coroutine a b e q f t)
    -> Coroutine a b e q f t
catch inner handler = Coroutine $
    \resolve acYield1 acError1 acPure1 onError onPure -> do
        runc inner resolve acYield1 acError1 acPure1
            (\acYield2 acError2 acPure2 err -> do
                runc (handler err) resolve acYield2 acError2 acPure2
                    onError onPure)
            onPure


suspend
    :: (a -> Coroutine a b e q f b)
    -> Coroutine a b e q f (Suspend a b e q)
suspend func = Coroutine $
    \resolve acYield1 acError1 acPure1 _ onPure -> do
        onPure acYield1 acError1 acPure1 $
            Suspend $ \acYield2 acError2 acPure2 inv -> do
                runc (func inv) resolve
                    acYield2
                    acError2
                    acPure2
                    (\_ acError3 _ err -> acError3 err)
                    (\_ _ acPure3 x -> acPure3 x)


resume
    :: Suspend a b e q
    -> (b -> Suspend a b e q -> Coroutine a b e q f t)
    -> (e -> Coroutine a b e q f t)
    -> (b -> Coroutine a b e q f t)
    -> a
    -> Coroutine a b e q f t
resume state1 handleYield handleError handlePure inv = Coroutine $
    \resolve acYield acError acPure onError onPure -> do
        runSuspend state1
            (\outv state2 -> runc (handleYield outv state2)
                resolve acYield acError acPure onError onPure)
            (\err -> runc (handleError err)
                resolve acYield acError acPure onError onPure)
            (\x -> runc (handlePure x)
                resolve acYield acError acPure onError onPure)
            inv


execute
    :: Coroutine a b e q f t
    -> (forall u . f u -> (u -> q) -> q)
    -> (e -> q)
    -> (t -> q)
    -> q
execute body resolve handleError handlePure = do
    runc body
        resolve
        (error "yield from the top level")
        (error "never used")
        (error "never used")
        (\_ _ _ err -> handleError err)
        (\_ _ _ x -> handlePure x)
