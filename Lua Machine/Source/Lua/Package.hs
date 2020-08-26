{-# LANGUAGE OverloadedStrings #-}


module Lua.Package (
    lpkLoadSource,
    lpkRequire,
    lpkSearchPath,
) where


import Data.Maybe
import System.IO
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Lua.Common


splitStr :: BSt.ByteString -> BSt.ByteString -> [BSt.ByteString]
splitStr sep source = do
    let (h, t) = BSt.breakSubstring sep source
    if BSt.null t
        then [h]
        else h:splitStr sep (BSt.drop (BSt.length sep) t)


lpkLoadSource
    :: LuaFunction q s
    -> LuaState q s B.ByteString
lpkLoadSource ff = do
    doLoad B.empty
    where
    doLoad buffer = do
        cret <- ff []
        case cret of
            [] -> return $ buffer
            LNil:_ -> return $ buffer
            LString bstr:_ -> do
                if BSt.null bstr
                    then return $ buffer
                    else do
                        doLoad (buffer <> B.fromStrict bstr)
            part:_ -> luaError $ LString $
                "Expected a string or a nil from the chunk reader, got a "
                    <> luaTypename part <> " instead"


lpkRequire
    :: LuaValue q s
    -> LuaValue q s
    -> LuaValue q s
    -> LuaState q s (LuaValue q s)
lpkRequire loaded searchers name = do
    modvalue <- luaRawGet loaded name
    case modvalue of
        LNil -> search [] 1
        _ -> return $ modvalue
    where
    search errs n = do
        searcher <- luaGet searchers (LInteger n)
        case searcher of
            LNil -> do
                let namestr = fromMaybe (BSt.pack $ show name) $
                        luaToString name
                luaError $ LString $ BSt.concat $
                    ("Failed to load module " <> namestr <> ":"):reverse errs
            _ -> do
                srets <- luaCall searcher [name]
                case srets of
                    LString msg:_ -> search (msg:errs) (n+1)
                    LNil:_ -> search errs (n+1)
                    [] -> search errs (n+1)
                    lres :| ldata :| _ -> do
                        mrets <- luaCall lres [ldata]
                        let modvalue = case mrets of
                                LNil:_ -> LBool True
                                a:_ -> a
                                [] -> LBool True
                        luaRawSet loaded name modvalue
                        return $ modvalue


lpkSearchPath
    :: BSt.ByteString
    -> BSt.ByteString
    -> BSt.ByteString
    -> BSt.ByteString
    -> BSt.ByteString
    -> LuaState q s (Either BSt.ByteString BSt.ByteString)
lpkSearchPath name prefix path sep rep = do
    let namerep = BSt.intercalate rep $ splitStr sep name
    let paths = filter (not . BSt.null) $ BSt.split ';' path
    let fnames = map (BSt.intercalate namerep . BSt.split '?') paths
    mfound <- trynames fnames
    case mfound of
        Nothing -> return $ Left $
            BSt.concat $
                map (\x -> "\nNo file " <> BSt.pack (show x)) $
                    fnames
        Just r -> return $ Right r
    where
    trynames [] = return $ Nothing
    trynames (fname:rest) = do
        result <- luaTry $
            luaLiftIO $
                withFile (BSt.unpack $ prefix <> fname) ReadMode (\_ -> do
                    return ())
        case result of
            Left _ -> trynames rest
            Right _ -> return $ Just $ prefix <> fname
