{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Lua.Table (
    Key(..),
    Nilable(..),
    Table,
    get,
    keys,
    length,
    new,
    set,
) where

import Prelude hiding (length)
import Control.Monad.ST
import Data.Hashable (Hashable)
import Data.Maybe
import Data.STRef
import GHC.Generics (Generic)
import qualified Data.Array.ST as V
import qualified Data.ByteString.Char8 as BSt
import qualified Data.Hashable ()
import qualified Data.HashTable.ST.Cuckoo as H


class Nilable v where
    nil :: v
    isNil :: v -> Bool
    -- isNil nil === True


data Key
    = KNil
    | KBool !Bool
    | KInt !Int
    | KNumber !Rational
    | KNegInf
    | KPosInf
    | KNaN
    | KString !BSt.ByteString
    | KId !Int
    deriving (Eq, Show, Generic)


instance Hashable Key


data TableData s v = TableData
    !Int
    !(V.STArray s Int v) -- always starts at 1
    !(H.HashTable s Key v) -- never contains any nils


newtype Table s v = Table (STRef s (TableData s v))


updateListPart
    :: (Nilable v)
    => TableData s v
    -> ST s (TableData s v)
updateListPart !table@(TableData listFill listPart hashPart) = do
    (_, oldcap) <- V.getBounds listPart
    let newcap = desiredCapacity oldcap
    if newcap /= oldcap
        then do
            newListPart <- V.newArray_ (1, newcap)
            let commoncap = min oldcap newcap
            _ <- copyElems (0::Int) (commoncap+1) oldcap
                (V.readArray listPart)
                (\i x -> if isNil x
                    then return ()
                    else H.insert hashPart (KInt i) x)
            counter1 <- copyElems 0 1 commoncap
                (V.readArray listPart)
                (V.writeArray newListPart)
            counter2 <- copyElems counter1 (commoncap+1) newcap
                (\i -> H.mutate hashPart (KInt i) $ \mvalue -> do
                    (Nothing, fromMaybe nil mvalue))
                (V.writeArray newListPart)
            return $! TableData counter2 newListPart hashPart
        else return $! table
    where
    copyElems !counter !i !maxi reader writer
        | i > maxi = return counter
        | otherwise = do
            value <- reader i
            () <- writer i value
            let counter2 = if isNil value
                    then counter
                    else counter + 1
            copyElems counter2 (i+1) maxi reader writer
    desiredCapacity !capacity
        {- when fill < 1/4 capacity, shrink, until the lowest capacity of 8 -}
        | 8 < capacity && capacity > 4*listFill = do
            desiredCapacity $ capacity `div` 2
        {- when fill > 3/4 capacity, grow -}
        | 4*listFill >= 3*capacity = do
            desiredCapacity $ capacity * 2
        | otherwise = do
            capacity


new
    :: (Nilable v)
    => ST s (Table s v)
new = do
    listPart <- V.newArray (1, 8) nil
    hashPart <- H.new
    ptable <- newSTRef $! TableData 0 listPart hashPart
    return $! Table ptable


get
    :: (Nilable v)
    => Table s v
    -> Key
    -> ST s v
get (Table ptable) !key = do
    TableData _ listPart hashPart <- readSTRef ptable
    case key of
        KNil -> return $ nil
        KInt i | 1 <= i -> do
            (_, cap) <- V.getBounds listPart
            if i <= cap
                then getListItem listPart i
                else getHashItem hashPart
        _ -> getHashItem hashPart
    where
    getListItem listPart i = do
        V.readArray listPart i
    getHashItem hashPart = do
        mr <- H.lookup hashPart key
        case mr of
            Just !r -> return $ r
            Nothing -> return $ nil


set
    :: (Nilable v)
    => Table s v
    -> Key
    -> v
    -> ST s ()
set (Table ptable) !key !value = do
    table@(TableData _ listPart hashPart) <- readSTRef ptable
    case key of
        KNil -> return ()
        KInt i | 1 <= i -> do
            (_, cap) <- V.getBounds listPart
            if i <= cap
                then setListItem i table
                else setHashItem hashPart
        _ -> setHashItem hashPart
    where
    setListItem index table@(TableData listFill listPart hashPart) = do
        oldvalue <- V.readArray listPart index
        V.writeArray listPart index value
        newtable <- case (isNil oldvalue, isNil value) of
            (False, True) -> updateListPart $!
                TableData (listFill-1) listPart hashPart
            (True, False) -> updateListPart $!
                TableData (listFill+1) listPart hashPart
            _ -> return $! table
        writeSTRef ptable $! newtable
    setHashItem hashPart = do
        if isNil value
            then H.delete hashPart key
            else H.insert hashPart key value


length
    :: (Nilable v)
    => Table s v
    -> ST s Int
length (Table ptable) = do
    table@(TableData _ listPart _) <- readSTRef ptable
    (_, cap) <- V.getBounds listPart
    upPhase table cap 0 1
    where
    upPhase table cap left right = do
        rv <- geti table cap right
        if isNil rv
            then downPhase table cap left right
            else if right < 0x40000000
                then upPhase table cap right (right*2)
                else downPhase table cap right (right*2-1)
    downPhase table cap left right
        | right - left <= 1 = return $ left
        | otherwise = do
            let mid = (right - left) `div` 2 + left
            mv <- geti table cap mid
            if isNil mv
                then downPhase table cap left mid
                else downPhase table cap mid right
    geti (TableData _ listPart hashPart) cap index = do
        if index <= cap
            then V.readArray listPart index
            else fromMaybe nil <$> H.lookup hashPart (KInt index)


keys
    :: (Nilable v)
    => Table s v
    -> ST s [Key]
keys (Table ptable) = do
    TableData _ listPart hashPart <- readSTRef ptable
    ks1 <- H.foldM
        (\ks (key, _) -> return $! key:ks)
        []
        hashPart
    (_, cap) <- V.getBounds listPart
    return $ map KInt [1..cap] ++ ks1


-- dump
    -- :: (Nilable v, Show v)
    -- => Table RealWorld v
    -- -> IO ()
-- dump (Table ptable) = do
    -- TableData listFill listPart hashPart <- stToIO $ readSTRef ptable
    -- len <- stToIO $ length $ Table ptable
    -- putStrLn $ "Table"
    -- putStrLn $ "  length: " ++ show len
    -- putStrLn $ "  fill: " ++ show listFill
    -- putStrLn $ "  listPart:"
    -- (minb, maxb) <- stToIO $ V.getBounds listPart
    -- putStrLn $ "    bounds: (" ++ show minb ++ ", " ++ show maxb ++ ")"
    -- forM_ [minb .. maxb] $ \i -> do
        -- value <- stToIO $ V.readArray listPart i
        -- putStrLn $ "    [KInt " ++ show i ++ "] -> " ++ show value
    -- putStrLn $ "  hashPart:"
    -- kvs <- stToIO $ H.foldM (\xs kv -> return $! kv:xs) [] hashPart
    -- forM_ kvs $ \(key, value) -> do
        -- putStrLn $ "    [" ++ show key ++ "] -> " ++ show value


-- alter
    -- :: (Nilable v, Show v)
    -- => Table RealWorld v
    -- -> (Table RealWorld v -> ST RealWorld a)
    -- -> IO a
-- alter table func = do
    -- x <- stToIO $ func table
    -- dump table
    -- return $ x


-- tt :: IO (Table RealWorld String)
-- tt = do
    -- t <- stToIO $ new
    -- dump t
    -- stToIO $ set t (KInt 3) "c"
    -- dump t
    -- stToIO $ set t (KInt 4) "d"
    -- dump t
    -- stToIO $ set t (KInt 1) "a"
    -- dump t
    -- stToIO $ set t (KInt 2) "b"
    -- dump t
    -- stToIO $ set t (KInt 6) "f"
    -- dump t
    -- stToIO $ set t (KInt 10) "z"
    -- dump t
    -- stToIO $ set t (KInt 5) "e"
    -- dump t
    -- stToIO $ set t (KString "foo") "zzf"
    -- dump t
    -- stToIO $ set t (KString "bat") "zzb"
    -- dump t
    -- stToIO $ set t (KId 0) "x0"
    -- dump t
    -- stToIO $ set t (KId 1) "x1"
    -- dump t
    -- stToIO $ set t (KId 2) "x2"
    -- dump t
    -- stToIO $ set t (KInt 3) ""
    -- dump t
    -- stToIO $ set t (KInt 4) ""
    -- dump t
    -- stToIO $ set t (KInt 1) ""
    -- dump t
    -- stToIO $ set t (KInt 2) ""
    -- dump t
    -- stToIO $ set t (KInt 6) ""
    -- dump t
    -- return $ t


-- instance Nilable [Char] where
    -- nil = []
    -- isNil = null
