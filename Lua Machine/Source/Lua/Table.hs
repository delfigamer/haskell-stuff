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
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Function
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
    -> ST s (V.STArray s Int v)
updateListPart !(TableData len listPart hashPart) = do
    (_, oldcap) <- V.getBounds listPart
    let descap = desiredCapacity oldcap
    case assert (descap >= len) () of
        _
            | descap < oldcap -> do
                newListPart <- V.mapIndices (1, descap) id listPart
                forM_ [descap+1 .. oldcap] $ \i -> do
                    value <- V.readArray listPart i
                    if isNil value
                        then return ()
                        else H.insert hashPart (KInt i) value
                return $! newListPart
            | descap > oldcap -> do
                newListPart <- V.newArray_ (1, descap)
                forM_ [1 .. oldcap] $ \i -> do
                    value <- V.readArray listPart i
                    V.writeArray newListPart i value
                forM_ [oldcap+1 .. descap] $ \i -> do
                    value <- H.mutate hashPart (KInt i) $ \mvalue -> do
                        (Nothing, fromMaybe nil mvalue)
                    V.writeArray newListPart i value
                return $! newListPart
            | otherwise -> do
                return $! listPart
    where
    desiredCapacity !capacity
        -- when length < 1/4 capacity, shrink, until the lowest capacity of 4
        | 4 < capacity && capacity > 4*len = do
            desiredCapacity $ capacity `div` 2
        -- when length > 3/4 capacity, grow
        | 4*len >= 3*capacity = do
            desiredCapacity $ capacity * 2
        | otherwise = do
            capacity


new
    :: (Nilable v)
    => ST s (Table s v)
new = do
    listPart <- V.newArray (1, 4) nil
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
                then if isNil value
                    then deleteListItem i table
                    else insertListItem i cap table
                else setHashItem hashPart
        _ -> setHashItem hashPart
    where
    insertListItem index oldcap (TableData oldlen oldListPart hashPart)
        | index > oldlen = do
            newlen <- flip fix index $ \loop !i -> do
                let nexti = i + 1
                if nexti <= oldcap
                    then do
                        nextval <- V.readArray oldListPart nexti
                        if isNil nextval
                            then return $ i
                            else loop $ nexti
                    else do
                        mval <- H.lookup hashPart (KInt nexti)
                        case mval of
                            Nothing -> return $ i
                            Just _ -> loop $ nexti
            listPart <- updateListPart $ TableData newlen oldListPart hashPart
            V.writeArray listPart index value
            writeSTRef ptable $! TableData newlen listPart hashPart
        | otherwise = do
            V.writeArray oldListPart index value
    deleteListItem index (TableData oldlen oldListPart hashPart)
        | index <= oldlen = do
            newlen <- flip fix (index-1) $ \loop !i -> do
                if i > 0
                    then do
                        prevval <- V.readArray oldListPart i
                        if isNil prevval
                            then loop $ i-1
                            else return $ i
                    else return $ 0
            listPart <- updateListPart $ TableData newlen oldListPart hashPart
            V.writeArray listPart index nil
            writeSTRef ptable $! TableData newlen listPart hashPart
        | otherwise = do
            V.writeArray oldListPart index nil
    setHashItem hashPart = do
        if isNil value
            then H.delete hashPart key
            else H.insert hashPart key value


length
    :: Table s v
    -> ST s Int
length (Table ptable) = do
    TableData len _ _ <- readSTRef ptable
    return $! len


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


-- newtype Iterator s k v = Iterator
    -- {step :: ST s (Maybe (k, v, Iterator s k v))}


-- ipairs
    -- :: (Nilable v)
    -- => Table s v
    -- -> Iterator s Int v
-- ipairs (Table ptable) = do
    -- Iterator (go 1)
    -- where
    -- go !index = do
        -- TableData _ listPart hashPart <- readSTRef ptable
        -- (_, cap) <- V.getBounds listPart
        -- let !nexti = index+1
        -- let !nextgo = Iterator $ go nexti
        -- if index <= cap
            -- then do
                -- value <- V.readArray listPart index
                -- if isNil value
                    -- then return $! Nothing
                    -- else return $! Just (index, value, nextgo)
            -- else do
                -- mvalue <- H.lookup hashPart (KInt index)
                -- case mvalue of
                    -- Nothing -> return $! Nothing
                    -- Just !value -> return $! Just (index, value, nextgo)


-- pairs
    -- :: (Nilable v)
    -- => Table s v
    -- -> Iterator s Key v
-- pairs (Table ptable) = do
    -- Iterator (goList 1)
    -- where
    -- goList !index = do
        -- TableData _ listPart _ <- readSTRef ptable
        -- (_, cap) <- V.getBounds listPart
        -- if index <= cap
            -- then do
                -- let !nexti = index+1
                -- value <- V.readArray listPart index
                -- if isNil value
                    -- then goList nexti
                    -- else do
                        -- let !nextgo = Iterator $ goList nexti
                        -- let !key = KInt index
                        -- return $! Just (key, value, nextgo)
            -- else walkList
    -- walkList = do
        -- TableData _ _ hashPart <- readSTRef ptable
        -- keys <- H.foldM
            -- (\ks (key, _) -> return $! key:ks)
            -- []
            -- hashPart
        -- goKeys hashPart keys
    -- goKeys hashPart !keys = do
        -- case keys of
            -- (!key):rest -> do
                -- mvalue <- H.lookup hashPart key
                -- case mvalue of
                    -- Nothing -> goKeys hashPart rest
                    -- Just !value -> do
                        -- let !nextgo = Iterator $ goKeys hashPart rest
                        -- return $! Just (key, value, nextgo)
            -- [] -> return $! Nothing


-- dump
    -- :: (Nilable v, Show v)
    -- => Table RealWorld v
    -- -> IO ()
-- dump (Table ptable) = do
    -- TableData len listPart hashPart <- stToIO $ readSTRef ptable
    -- putStrLn $ "Table"
    -- putStrLn $ "  length: " ++ show len
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
    -- t <- stToIO $ do
        -- t <- new
        -- set t (KInt 3) "c"
        -- set t (KInt 4) "d"
        -- set t (KInt 10) "a"
        -- set t (KInt 20) "b"
        -- set t (KInt 60) "f"
        -- set t (KInt 50) "e"
        -- set t (KString "foo") "zzf"
        -- set t (KString "bat") "zzb"
        -- set t (KId 0) "x0"
        -- set t (KId 1) "x1"
        -- set t (KId 2) "x2"
        -- return $ t
    -- flip fix (pairs t) $ \loop iter -> do
        -- out <- stToIO $ step iter
        -- case out of
            -- Just (key, value, next) -> do
                -- putStrLn $ "[" ++ show key ++ "] -> " ++ show value
                -- loop next
            -- Nothing -> return ()
    -- return $ t


-- instance Nilable [Char] where
    -- nil = []
    -- isNil = null
