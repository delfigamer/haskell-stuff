{-# LANGUAGE OverloadedStrings #-}


module Lua.Lib (
    lualibs,
) where


import Control.Monad
import Data.Ratio
import qualified Data.ByteString.Char8 as BSt
import Lua.Common
import Lua.Debug
import Lua.SourceRange


lualibs :: LuaState q s (LuaValue q s)
lualibs = do
    io <- luaCreateTable =<< mapM makef [
        ("write", (\args -> do
            luaLiftIO $
                forM_ args (\arg -> do
                    case arg of
                        LString str -> BSt.putStr str
                        _ -> putStr $ show arg)
            return $ []))]
    math <- luaCreateTable =<< mapM makef [
        ("abs", (\args -> do
            let a:_ = luaExtend 1 args
            case a of
                LInteger x -> return $ [LInteger $ abs x]
                LRational x -> return $ [LRational $ abs x]
                LDouble x -> return $ [LDouble $ abs x]
                _ -> luaError $ errArgType 1 "number" a)),
        ("exact", (\args -> do
            let a:_ = luaExtend 1 args
            case a of
                LInteger x -> return $ [a]
                LRational x -> return $ [a]
                LDouble x -> if isNaN x || isInfinite x
                    then return $ [a]
                    else do
                        let rat = toRational x
                        if denominator rat == 1
                            then return $ [LInteger $ numerator rat]
                            else return $ [LRational $ rat]
                _ -> luaError $ errArgType 1 "number" a)),
        ("inexact", (\args -> do
            let a:_ = luaExtend 1 args
            case a of
                LInteger x -> return $ [LDouble $ fromInteger x]
                LRational x -> return $ [LDouble $ fromRational x]
                LDouble x -> return $ [a]
                _ -> luaError $ errArgType 1 "number" a))]
    elems <- mapM makef []
    luaCreateTable (
        (LString "io", io)
        :(LString "math", math)
        :elems)
    where
    makef
        :: (BSt.ByteString, LuaFunction q s)
        -> LuaState q s (LuaValue q s, LuaValue q s)
    makef (name, func) = do
        fv <- luaCreateFunction func
        return $ (luaPush name, fv)
