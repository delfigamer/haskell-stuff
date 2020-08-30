{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Test.Test (
    testLua,
    runLua,
    runLuaA,
) where


import Data.Maybe
import qualified Data.ByteString.Char8 as BSt
import qualified Data.ByteString.Lazy.Char8 as B
import Lua


data WithTraceback q s = WithTraceback
    (LuaValue q s)
    [(Maybe BSt.ByteString, Maybe SourceRange)]


instance LuaMetatype WithTraceback where
    lmtAsString (WithTraceback msgv tb) _ = do
        msg <- luaAsString msgv
        let tblines = map
                (\(mname, mpr) -> do
                    let name = fromMaybe "" mname
                    let prs = maybe "" (BSt.pack . show) mpr
                    name <> "\t" <> prs)
                tb
        return $ BSt.intercalate "\n" (msg:filterrep False "" tblines)
        where
        filterrep reps _ []
            | reps = ["\t..."]
            | otherwise = []
        filterrep reps prev (current:rest)
            | prev == current = filterrep True prev rest
            | reps = "\t\t...":current:filterrep False current rest
            | otherwise = current:filterrep False current rest


grabTraceback :: LuaErrorHandler q s
grabTraceback msg = do
    tb <- luadGetTraceback
    luaCreateUserdata $ WithTraceback msg tb


testAssert :: LuaValue q s -> LuaState q s ()
testAssert env = do
    check "return assert(true)" (\r -> do
        case r of
            Right [LBool True] -> True
            _ -> False)
    check "return assert('text')" (\r -> do
        case r of
            Right [LString "text"] -> True
            _ -> False)
    check "return assert(false, 'message')" (\r -> do
        case r of
            Left err -> do
                let mwt = luaToUserdata err
                case mwt of
                    Just (WithTraceback (LString "message") _) -> True
                    _ -> False
            _ -> False)
    check "return error('errmsg')" (\r -> do
        case r of
            Left err -> do
                let mwt = luaToUserdata err
                case mwt of
                    Just (WithTraceback (LString "errmsg") _) -> True
                    _ -> False
            _ -> False)
    where
    check source tf = do
        r <- luaTry $ luaDo source env []
        if tf r
            then return ()
            else do
                luaLiftIO $
                    putStrLn $
                        "fail: " ++ show source ++ " produced " ++ show r


testLua :: IO ()
testLua = do
    r <- luaRunIO $ luaWithErrHandler grabTraceback (do
        env <- lualibs
        testAssert env
        luaDo_ "_U = true" env []
        luaDo_ "package.prefix = 'lua-tests/'" env []
        let path = "lua-tests/all.lua"
        input <- luaLiftIO $ B.readFile path
        lret <- luaLoad path input env
        case lret of
            Left err -> luaError err
            Right chunk -> luaCall_ chunk []
        return ())
    case r of
        Left e -> putStrLn $ e
        Right _ -> return ()


runLuaA
    :: String
    -> (forall q s . [LuaValue q s])
    -> IO ()
runLuaA input args = do
    r <- luaRunIO $ luaWithErrHandler grabTraceback $ do
        env <- lualibs
        r <- luaDo (B.pack input) env args
        luaLiftIO $ print r
        return ()
    case r of
        Left e -> putStrLn $ e
        Right _ -> return ()


runLua :: String -> IO ()
runLua input = runLuaA input []
